#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

#include "til_parser.tab.h"

/**
 * Handle conversion of a node to a covariant type.
 *
 * @param target_type the type to convert to
 * @param node the node that might need conversion
 * @param lvl the AST traversal depth level
 */
void til::postfix_writer::convertCovariantNode(std::shared_ptr<cdk::basic_type> const &target_type,
                                               cdk::expression_node *const node, int lvl) {
    if (target_type->name() != cdk::TYPE_FUNCTIONAL || !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
        node->accept(this, lvl);
        if (target_type->name() == cdk::TYPE_DOUBLE && node->is_typed(cdk::TYPE_INT)) {
            _pf.I2D();
        }
        return;
    }

    std::shared_ptr<cdk::functional_type> r_type = cdk::functional_type::cast(node->type());
    std::shared_ptr<cdk::functional_type> l_type = cdk::functional_type::cast(target_type);
    bool hasCovariantTypes = false;

    // If either the return types or any of the argument types are covariant
    if (l_type->output(0)->name() == cdk::TYPE_DOUBLE && r_type->output(0)->name() == cdk::TYPE_INT) {
        hasCovariantTypes = true;
    } else {
        for (size_t i = 0; i < l_type->input_length(); i++) {
            if (l_type->input(i)->name() == cdk::TYPE_INT && r_type->input(i)->name() == cdk::TYPE_DOUBLE) {
                hasCovariantTypes = true;
                break;
            }
        }
    }

    if (!hasCovariantTypes) {
        // nothing to do
        node->accept(this, lvl);
        return;
    }

    // arguments and/or return need conversion
    int lineno = node->lineno();
    std::string aux_global_decl_name = "_aux_global_" + std::to_string(_lbl++);
    auto aux_global_decl = new til::variable_declaration_node(lineno, tPRIVATE, r_type, aux_global_decl_name,
                                                              nullptr);
    auto aux_global_var = new cdk::variable_node(lineno, aux_global_decl_name);

    _forceGlobalFutureDecls = true;
    aux_global_decl->accept(this, lvl);
    _forceGlobalFutureDecls = false;

    inFunction() ? _pf.TEXT(_functionLabels.top()) : _pf.DATA();
    _pf.ALIGN();

    auto aux_global_assignment = new cdk::assignment_node(lineno, aux_global_var, node);
    aux_global_assignment->accept(this, lvl);
    auto aux_global_rvalue = new cdk::rvalue_node(lineno, aux_global_var);

    auto args = new cdk::sequence_node(lineno);
    auto call_args = new cdk::sequence_node(lineno);
    for (size_t i = 0; i < l_type->input_length(); i++) {
        auto arg_name = "_arg" + std::to_string(i);

        auto arg_decl = new til::variable_declaration_node(lineno, tPRIVATE, l_type->input(i), arg_name, nullptr);
        args = new cdk::sequence_node(lineno, arg_decl, args);

        auto arg_rvalue = new cdk::rvalue_node(lineno, new cdk::variable_node(lineno, arg_name));
        call_args = new cdk::sequence_node(lineno, arg_rvalue, call_args);
    }

    auto function_call = new til::function_call_node(lineno, aux_global_rvalue, call_args);
    auto return_node = new til::return_node(lineno, function_call);
    auto block = new til::block_node(lineno, new cdk::sequence_node(lineno),
                                     new cdk::sequence_node(lineno, return_node));

    (new til::function_node(lineno, args, block, l_type->output(0), false))->accept(this, lvl);
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_with_node(til::with_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    _pf.ALIGN();
    auto lineno = node->lineno();

    std::string it_name = "_it";
    auto it_init = dynamic_cast<cdk::integer_node *>(node->low());
    auto it_decl = new til::variable_declaration_node(lineno, tPRIVATE, cdk::primitive_type::create(4, cdk::TYPE_INT), it_name, it_init);

    // Declarar iterador
    it_decl->accept(this, lvl + 2);

    auto it_var = new cdk::variable_node(lineno, it_name);
    auto it_rvalue = new cdk::rvalue_node(lineno, it_var);
    auto comparison = new cdk::le_node(lineno, it_rvalue, node->high());

    auto vec_el = new til::index_node(lineno, node->vector(), it_rvalue);
    auto vec_el_rvalue = new cdk::rvalue_node(lineno, vec_el);
    auto function_call = new til::function_call_node(lineno, node->func(), new cdk::sequence_node(lineno, vec_el_rvalue));

    auto inc_literal = new cdk::integer_node(lineno, 1);
    auto inc_add = new cdk::add_node(lineno, it_rvalue, inc_literal);
    auto inc_it = new cdk::assignment_node(lineno, it_var, inc_add);
    auto inc_eval = new til::evaluation_node(lineno, inc_it);

    auto loop_body = new cdk::sequence_node(lineno, function_call);
    loop_body = new cdk::sequence_node(lineno, inc_eval, loop_body);

    auto iterate_loop = new til::while_node(lineno, comparison, loop_body);
    iterate_loop->accept(this, lvl + 2);

    _pf.ALIGN();
}

void til::postfix_writer::do_nil_node(cdk::nil_node *const node, int lvl) {
    // EMPTY
}

void til::postfix_writer::do_data_node(cdk::data_node *const node, int lvl) {
    // EMPTY
}

void til::postfix_writer::do_double_node(cdk::double_node *const node, int lvl) {
    inFunction() ? _pf.DOUBLE(node->value()) : _pf.SDOUBLE(node->value());
}

void til::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl + 2);
    _pf.INT(0);
    _pf.EQ();
}

void til::postfix_writer::do_and_node(cdk::and_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    int lbl = ++_lbl;
    node->left()->accept(this, lvl);
    _pf.DUP32();
    // optimization - if the value of left == 0, we skip the right comparison
    _pf.JZ(mklbl(lbl));
    node->right()->accept(this, lvl);
    _pf.AND();
    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl));
}

void til::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    int lbl = ++_lbl;
    node->left()->accept(this, lvl);
    _pf.DUP32();
    // optimization - if the value of left != 0, we skip the right comparison
    _pf.JNZ(mklbl(lbl));
    node->right()->accept(this, lvl);
    _pf.OR();
    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node *const node, int lvl) {
    for (size_t i = 0; i < node->size(); i++) {
        node->node(i)->accept(this, lvl);
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node *const node, int lvl) {
    inFunction() ? _pf.INT(node->value()) : _pf.SINT(node->value());
}

void til::postfix_writer::do_string_node(cdk::string_node *const node, int lvl) {
    int lbl1;

    _pf.RODATA(); // select segment for constant objects (string literal)
    _pf.ALIGN(); // align memory
    _pf.LABEL(mklbl(lbl1 = ++_lbl)); // define the label
    _pf.SSTRING(node->value()); // allocate space by defining initial value

    if (inFunction()) {
        _pf.TEXT(_functionLabels.top()); // return to the TEXT segment
        _pf.ADDR(mklbl(lbl1)); // the string to be stored
    } else {
        _pf.DATA(); // select segment for initialized variables
        _pf.SADDR(mklbl(lbl1));
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    node->argument()->accept(this, lvl); // get the value

    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.DNEG() : _pf.NEG();
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    node->argument()->accept(this, lvl); // get the value
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_add_node(cdk::add_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
        // Convert the integer to a double.
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
        // Pointer arithmetics
        std::shared_ptr<cdk::reference_type> ref = cdk::reference_type::cast(node->type());
        _pf.INT(ref->referenced()->size() > 1 ? ref->referenced()->size() : 1);
        _pf.MUL(); // Multiply the int by the size of the pointer's referred type
    }

    node->right()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
        // Pointer arithmetics
        std::shared_ptr<cdk::reference_type> ref = cdk::reference_type::cast(node->type());
        _pf.INT(ref->referenced()->size() > 1 ? ref->referenced()->size() : 1);
        _pf.MUL(); // Multiply the int by the size of the pointer's referred type
    }

    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.DADD() : _pf.ADD();
}

void til::postfix_writer::do_sub_node(cdk::sub_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
        // Pointer arithmetics
        std::shared_ptr<cdk::reference_type> ref = cdk::reference_type::cast(node->type());
        _pf.INT(ref->referenced()->size() > 1 ? ref->referenced()->size() : 1);
        _pf.MUL(); // Multiply the int by the size of the pointer's referred type
    }

    node->right()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
        // Pointer arithmetics
        std::shared_ptr<cdk::reference_type> ref = cdk::reference_type::cast(node->type());
        _pf.INT(ref->referenced()->size() > 1 ? ref->referenced()->size() : 1);
        _pf.MUL(); // Multiply the int by the size of the pointer's referred type
    }

    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.DSUB() : _pf.SUB();

    if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
        // difference between two pointers => divide by the size of the type they refer
        std::shared_ptr<cdk::reference_type> ref = cdk::reference_type::cast(node->left()->type());
        _pf.INT(ref->referenced()->size() > 1 ? ref->referenced()->size() : 1);
        _pf.DIV();
    }
}

void til::postfix_writer::convertBinaryExpressionIntDoubleArgs(cdk::binary_operation_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->left()->accept(this, lvl);
    if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.I2D();
    }

    node->right()->accept(this, lvl);
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
    }
}

void til::postfix_writer::do_mul_node(cdk::mul_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.DMUL() : _pf.MUL();
}

void til::postfix_writer::do_div_node(cdk::div_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.DDIV() : _pf.DIV();
}

void til::postfix_writer::do_mod_node(cdk::mod_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    node->left()->accept(this, lvl);
    node->right()->accept(this, lvl);
    _pf.MOD();
}

/**
 * For all the comparison nodes (lt, le, gt...) we start by converting he required values.
 * If any of the arguments are of type double, we compare these doubles using DCMP and an auxiliary 0 value.
 * We then do the standard integer comparison (if we have doubles, the comparison is between the DCMP result and the aux 0)
 * @param node
 * @param lvl
 */
void til::postfix_writer::do_lt_node(cdk::lt_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DCMP();
        _pf.INT(0);
    }
    _pf.LT();
}

void til::postfix_writer::do_le_node(cdk::le_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DCMP();
        _pf.INT(0);
    }
    _pf.LE();
}

void til::postfix_writer::do_ge_node(cdk::ge_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DCMP();
        _pf.INT(0);
    }
    _pf.GE();
}

void til::postfix_writer::do_gt_node(cdk::gt_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DCMP();
        _pf.INT(0);
    }
    _pf.GT();
}

void til::postfix_writer::do_ne_node(cdk::ne_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DCMP();
        _pf.INT(0);
    }
    _pf.NE();
}

void til::postfix_writer::do_eq_node(cdk::eq_node *const node, int lvl) {
    convertBinaryExpressionIntDoubleArgs(node, lvl);
    if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DCMP();
        _pf.INT(0);
    }
    _pf.EQ();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    std::shared_ptr<til::symbol> symbol = _symtab.find(node->name());

    if (symbol->qualifier() == tEXTERNAL) {
        _externalFunctionName = symbol->name();
    } else if (symbol->global()) {
        _pf.ADDR(node->name());
    } else {
        _pf.LOCAL(symbol->offset());
    }
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    node->lvalue()->accept(this, lvl);

    if (_externalFunctionName) {
        return;
    }

    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.LDDOUBLE() : _pf.LDINT();
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    convertCovariantNode(node->type(), node->rvalue(), lvl);
    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.DUP64() : _pf.DUP32();

    node->lvalue()->accept(this, lvl);
    // store the value
    node->is_typed(cdk::TYPE_DOUBLE) ? _pf.STDOUBLE() : _pf.STINT();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->argument()->accept(this, lvl);

    if (node->argument()->type()->size() > 0) {
        _pf.TRASH(node->argument()->type()->size());
    }
}

void til::postfix_writer::do_print_node(til::print_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
        auto child = dynamic_cast<cdk::expression_node *>(node->arguments()->node(ix));

        child->accept(this, lvl); // expression to print
        if (child->is_typed(cdk::TYPE_INT)) {
            _externalFunctionsToDeclare.insert("printi");
            _pf.CALL("printi");
            _pf.TRASH(4); // delete the printed value
        } else if (child->is_typed(cdk::TYPE_DOUBLE)) {
            _externalFunctionsToDeclare.insert("printd");
            _pf.CALL("printd");
            _pf.TRASH(8); // delete the printed value
        } else if (child->is_typed(cdk::TYPE_STRING)) {
            _externalFunctionsToDeclare.insert("prints");
            _pf.CALL("prints");
            _pf.TRASH(4); // delete the printed value's address
        } else {
            THROW_ERROR("cannot print expression of unknown type");
        }
    }

    if (node->newline()) {
        _externalFunctionsToDeclare.insert("println");
        _pf.CALL("println");
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_read_node(til::read_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    if (node->is_typed(cdk::TYPE_DOUBLE)) {
        _externalFunctionsToDeclare.insert("readd");
        _pf.CALL("readd");
        _pf.LDFVAL64();
    } else if (node->is_typed(cdk::TYPE_INT)) {
        _externalFunctionsToDeclare.insert("readi");
        _pf.CALL("readi");
        _pf.LDFVAL32();
    } else {
        THROW_ERROR("cannot read type");
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sizeof_node(til::sizeof_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    _pf.INT(node->expression()->type()->size());
}

void til::postfix_writer::do_null_node(til::null_node *const node, int lvl) {
    inFunction() ? _pf.INT(0) : _pf.SINT(0);
}

void til::postfix_writer::do_index_node(til::index_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    node->base()->accept(this, lvl + 2);
    node->index()->accept(this, lvl + 2);
    _pf.INT(node->type()->size());
    _pf.MUL();
    _pf.ADD();
}

void til::postfix_writer::do_address_of_node(til::address_of_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    node->lvalue()->accept(this, lvl + 2);
}

void til::postfix_writer::do_alloc_node(til::alloc_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    std::shared_ptr<cdk::basic_type> referenced = cdk::reference_type::cast(node->type())->referenced();
    node->argument()->accept(this, lvl);
    _pf.INT(referenced->size() > 1 ? referenced->size() : 1);
    _pf.MUL();
    _pf.ALLOC();
    _pf.SP();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_function_node(til::function_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    std::string functionLabel;
    node->is_main() ? functionLabel = "_main" : functionLabel = mklbl(++_lbl);

    _functionLabels.push(functionLabel);

    _pf.TEXT(_functionLabels.top());
    _pf.ALIGN();
    if (node->is_main()) {
        _pf.GLOBAL("_main", _pf.FUNC());
    }
    _pf.LABEL(_functionLabels.top());

    int oldOffset = _offset;
    _offset = 8; // function arguments start at offset 8
    _symtab.push();

    _inFunctionArgs = true;
    node->arguments()->accept(this, lvl);
    _inFunctionArgs = false;

    // compute stack size to be reserved for local variables
    frame_size_calculator fsc(_compiler, _symtab);
    node->block()->accept(&fsc, lvl);
    _pf.ENTER(fsc.localsize());

    auto oldFunctionRetLabel = _currentFunctionRetLabel;
    _currentFunctionRetLabel = mklbl(++_lbl);

    auto oldFunctionLoopLabels = _currentFunctionLoops;
    _currentFunctionLoops = new std::vector<std::pair<std::string, std::string>>();

    _offset = 0; // local variables start at offset 0

    node->block()->accept(this, lvl);

    if (node->is_main()) {
        // return 0 if main has no return statement
        _pf.INT(0);
        _pf.STFVAL32();
    }

    _pf.ALIGN();
    _pf.LABEL(_currentFunctionRetLabel);
    _pf.LEAVE();
    _pf.RET();

    delete _currentFunctionLoops;
    _currentFunctionLoops = oldFunctionLoopLabels;
    _currentFunctionRetLabel = oldFunctionRetLabel;
    _offset = oldOffset;
    _symtab.pop();
    _functionLabels.pop();

    if (node->is_main()) {
        for (const auto &name: _externalFunctionsToDeclare) {
            _pf.EXTERN(name);
        }
        return;
    }

    // Since a function (not main) is an expression, we push its address to the stack.
    if (inFunction()) {
        _pf.TEXT(_functionLabels.top());
        _pf.ADDR(functionLabel);
    } else {
        _pf.DATA();
        _pf.SADDR(functionLabel);
    }
}

void til::postfix_writer::do_return_node(til::return_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    auto symbol = _symtab.find("@", 1);
    auto rettype = cdk::functional_type::cast(symbol->type())->output(0);
    auto rettype_name = rettype->name();

    if (rettype_name != cdk::TYPE_VOID) {
        convertCovariantNode(rettype, node->retval(), lvl + 2);
        rettype_name == cdk::TYPE_DOUBLE ? _pf.STFVAL64() : _pf.STFVAL32();
    }

    _pf.JMP(_currentFunctionRetLabel);

    _visitedFinalInstruction = true;
}

//---------------------------------------------------------------------------

/** @tparam P index for loop labels pair */
template<size_t P, typename T>
void til::postfix_writer::executeLoopControlInstruction(T *const node) {
    ASSERT_SAFE_EXPRESSIONS;

    auto level = static_cast<size_t>(node->level());

    if (level == 0) {
        THROW_ERROR("invalid loop control instruction level");
    } else if (_currentFunctionLoops->size() < level) {
        THROW_ERROR("loop control instruction not within sufficient loops (expected at most " +
                    std::to_string(_currentFunctionLoops->size()) + ")");
    }

    auto index = _currentFunctionLoops->size() - level;
    auto label = std::get<P>(_currentFunctionLoops->at(index));
    _pf.JMP(label);

    _visitedFinalInstruction = true;
}

void til::postfix_writer::do_while_node(til::while_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    int condLabel, endLabel;

    _pf.ALIGN();
    condLabel = ++_lbl;
    _pf.LABEL(mklbl(condLabel));
    node->condition()->accept(this, lvl);
    endLabel = ++_lbl;
    _pf.JZ(mklbl(endLabel));

    _currentFunctionLoops->push_back(std::make_pair(mklbl(condLabel), mklbl(endLabel)));
    node->block()->accept(this, lvl + 2);
    _visitedFinalInstruction = false;
    _currentFunctionLoops->pop_back();

    _pf.JMP(mklbl(condLabel));
    _pf.ALIGN();
    _pf.LABEL(mklbl(endLabel));
}

void til::postfix_writer::do_next_node(til::next_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    size_t level = node->level();

    if (level == 0) {
        THROW_ERROR("invalid next instruction level");
    } else if (_currentFunctionLoops->size() < level) {
        THROW_ERROR("next instruction not within sufficient loops");
    }

    auto index = _currentFunctionLoops->size() - level;
    auto label = _currentFunctionLoops->at(index).first;
    _pf.JMP(label);

    _visitedFinalInstruction = true;
}

void til::postfix_writer::do_stop_node(til::stop_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    size_t level = node->level();

    if (level == 0) {
        THROW_ERROR("invalid stop instruction level");
    } else if (_currentFunctionLoops->size() < level) {
        THROW_ERROR("stop instruction not within sufficient loops");
    }

    auto index = _currentFunctionLoops->size() - level;
    auto label = _currentFunctionLoops->at(index).second;
    _pf.JMP(label);

    _visitedFinalInstruction = true;
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    int lbl1;
    node->condition()->accept(this, lvl);
    _pf.JZ(mklbl(lbl1 = ++_lbl));
    node->block()->accept(this, lvl + 2);
    _visitedFinalInstruction = false;
    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_else_node(til::if_else_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    int lbl1, lbl2;
    node->condition()->accept(this, lvl);
    _pf.JZ(mklbl(lbl1 = ++_lbl));
    node->thenblock()->accept(this, lvl + 2);
    _visitedFinalInstruction = false;
    _pf.JMP(mklbl(lbl2 = ++_lbl));
    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl1));
    node->elseblock()->accept(this, lvl + 2);
    _visitedFinalInstruction = false;
    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_declaration_node(til::variable_declaration_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;
    std::shared_ptr<til::symbol> symbol = new_symbol();
    reset_new_symbol();

    int offset = 0;
    int typesize = node->type()->size(); // in bytes
    if (_inFunctionArgs) {
        offset = _offset;
        _offset += typesize;
    } else if (inFunction()) {
        _offset -= typesize;
        offset = _offset;
    } else {
        // global variable
        offset = 0;
    }
    symbol->offset(offset);

    if (inFunction()) {
        // nothing to do for function args or local variables without initializer
        if (_inFunctionArgs || node->initializer() == nullptr) {
            return;
        }

        convertCovariantNode(node->type(), node->initializer(), lvl);
        if (node->is_typed(cdk::TYPE_DOUBLE)) {
            _pf.LOCAL(symbol->offset());
            _pf.STDOUBLE();
        } else {
            _pf.LOCAL(symbol->offset());
            _pf.STINT();
        }

        return;
    }

    if (symbol->qualifier() == tFORWARD || symbol->qualifier() == tEXTERNAL) {
        _externalFunctionsToDeclare.insert(symbol->name());
        return;
    }

    _externalFunctionsToDeclare.erase(symbol->name());

    if (node->initializer() == nullptr) {
        _pf.BSS();
        _pf.ALIGN();

        if (symbol->qualifier() == tPUBLIC) {
            _pf.GLOBAL(symbol->name(), _pf.OBJ());
        }

        _pf.LABEL(symbol->name());
        _pf.SALLOC(typesize);
        return;
    }

    if (!instanceOf<cdk::integer_node>(node->initializer()) &&
        !instanceOf<cdk::double_node>(node->initializer()) &&
        !instanceOf<cdk::string_node>(node->initializer()) &&
        !instanceOf<til::null_node>(node->initializer()) &&
        !instanceOf<til::function_node>(node->initializer())) {
        THROW_ERROR("non-literal initializer for global variable '" + symbol->name() + "'");
    }

    _pf.DATA();
    _pf.ALIGN();

    if (symbol->qualifier() == tPUBLIC) {
        _pf.GLOBAL(symbol->name(), _pf.OBJ());
    }

    _pf.LABEL(symbol->name());

    if (node->is_typed(cdk::TYPE_DOUBLE) && node->initializer()->is_typed(cdk::TYPE_INT)) {
        auto int_node = dynamic_cast<cdk::integer_node *>(node->initializer());
        // We make a double, even if the initializer is an int.
        _pf.SDOUBLE(int_node->value());
    } else {
        node->initializer()->accept(this, lvl);
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_function_call_node(til::function_call_node *const node, int lvl) {
    ASSERT_SAFE_EXPRESSIONS;

    std::shared_ptr<cdk::functional_type> func_type;
    if (node->func() == nullptr) { // recursive call
        auto symbol = _symtab.find("@", 1);
        func_type = cdk::functional_type::cast(symbol->type());
    } else { // "regular" call
        func_type = cdk::functional_type::cast(node->func()->type());
    }

    size_t argc = 0;
    // arguments visited in reverse order so the first argument is on top of the stack
    for (size_t i = node->arguments()->size(); i > 0; i--) {
        auto arg = dynamic_cast<cdk::expression_node *>(node->arguments()->node(i - 1));

        argc += arg->type()->size();
        convertCovariantNode(func_type->input(i - 1), arg, lvl + 2);
    }

    _externalFunctionName = std::nullopt;
    if (node->func() == nullptr) { // recursive call; "@"
        _pf.ADDR(_functionLabels.top());
    } else {
        node->func()->accept(this, lvl);
    }

    if (_externalFunctionName) {
        _pf.CALL(*_externalFunctionName);
        _externalFunctionName = std::nullopt;
    } else {
        _pf.BRANCH();
    }

    if (argc > 0) {
        _pf.TRASH(argc);
    }

    if (node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.LDFVAL64();
    } else if (!node->is_typed(cdk::TYPE_VOID)) {
        _pf.LDFVAL32();
    }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_block_node(til::block_node *const node, int lvl) {
    _symtab.push();
    node->declarations()->accept(this, lvl + 2);

    _visitedFinalInstruction = false;
    for (size_t i = 0; i < node->instructions()->size(); i++) {
        auto child = node->instructions()->node(i);

        if (_visitedFinalInstruction) {
            THROW_ERROR_FOR_NODE(child, "unreachable code");
        }

        child->accept(this, lvl + 2);
    }
    _visitedFinalInstruction = false;
    _symtab.pop();
}

//---------------------------------------------------------------------------

