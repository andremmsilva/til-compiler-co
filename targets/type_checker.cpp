#include <string>
#include <utility>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#include "til_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

/**
 * @brief
 *
 * @param left
 * @param right
 * @return uint64_t - type of both nodes (if they are equal) or UNSPEC
 */
inline uint64_t til::type_checker::compareTypesShallow(
    std::shared_ptr<cdk::basic_type> left,
    std::shared_ptr<cdk::basic_type> right
) {
    return left->name() == right->name() ? left->name() : cdk::TYPE_UNSPEC;
}

bool til::type_checker::compareTypes(
    std::shared_ptr<cdk::basic_type> left,
    std::shared_ptr<cdk::basic_type> right,
    bool similarTypesAllowed
) {
    if (left->name() == cdk::TYPE_UNSPEC || right->name() == cdk::TYPE_UNSPEC) {
        return false;
    }

    const auto result = compareTypesShallow(left, right);

    if (result == cdk::TYPE_UNSPEC) {
        return false;
    }

    if (result == cdk::TYPE_FUNCTIONAL) {
        auto left_func = cdk::functional_type::cast(left);
        auto right_func = cdk::functional_type::cast(right);

        if (left_func->input_length() != right_func->input_length()
            || left_func->output_length() != right_func->output_length()) {
            return false;
        }

        for (size_t i = 0; i < left_func->input_length(); i++) {
            if (!compareTypes(right_func->input(i), left_func->input(i), similarTypesAllowed)) {
                return false;
            }
        }

        for (size_t i = 0; i < left_func->output_length(); i++) {
            if (!compareTypes(left_func->output(i), right_func->output(i), similarTypesAllowed)) {
                return false;
            }
        }

        return true;
    }

    if (result == cdk::TYPE_POINTER) {
        return compareTypes(cdk::reference_type::cast(left)->referenced(), cdk::reference_type::cast(right)->referenced(), false);
    }

    if (similarTypesAllowed && left->name() == cdk::TYPE_DOUBLE) {
        return right->name() == cdk::TYPE_DOUBLE || right->name() == cdk::TYPE_INT;
    }

    return left == right;
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node* const node, int lvl) {
    for (size_t i = 0; i < node->size(); i++) {
        node->node(i)->accept(this, lvl);
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node* const node, int lvl) {
    // EMPTY
}

void til::type_checker::do_data_node(cdk::data_node* const node, int lvl) {
    // EMPTY
}

void til::type_checker::do_with_node(til::with_node *const node, int lvl) {
    node->func()->accept(this, lvl + 4);
    if(!node->func()->is_typed(cdk::TYPE_FUNCTIONAL)) {
        throw std::string("wrong 1st argument for with node - must be a function");
    }

    node->vector()->accept(this, lvl + 4);
    if(!node->vector()->is_typed(cdk::TYPE_POINTER)) {
        throw std::string("wrong 2nd argument for with node - must be a pointer");
    }

    auto func = cdk::functional_type::cast(node->func()->type());
    auto pointed = cdk::reference_type::cast(node->vector()->type())->referenced();
    if(func->output(0)->name() != cdk::TYPE_VOID) {
        throw std::string("wrong return type for function of with instruction");
    }
    if(func->input_length() != 1) {
        throw std::string("invalid arguments for function of with instruction");
    }
    if(!compareTypes(func->input(0), pointed, true)) {
        throw std::string("invalid arguments for with instruction");
    }

    node->low()->accept(this, lvl + 4);
    if (node->low()->is_typed(cdk::TYPE_UNSPEC)) {
        node->low()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->low()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in low of with instruction");
    }

    node->high()->accept(this, lvl + 4);
    if (node->high()->is_typed(cdk::TYPE_UNSPEC)) {
        node->high()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->high()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in high of with instruction");
    }
}

//---------------------------------------------------------------------------
//--- Data types ------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node* const node, int lvl) {
    ASSERT_UNSPEC;
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_double_node(cdk::double_node* const node, int lvl) {
    ASSERT_UNSPEC;
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void til::type_checker::do_string_node(cdk::string_node* const node, int lvl) {
    ASSERT_UNSPEC;
    node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------
//--- Unary expressions -----------------------------------------------------

void til::type_checker::processUnaryExpression(cdk::unary_operation_node* const node, int lvl, bool validForDoubles) {
    ASSERT_UNSPEC;

    node->argument()->accept(this, lvl + 2);

    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    if (!node->argument()->is_typed(cdk::TYPE_INT)) {
        if (!(validForDoubles && node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
            throw std::string("wrong type in argument of unary expression");
        }
    }

    node->type(node->argument()->type());
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node* const node, int lvl) {
    processUnaryExpression(node, lvl, true);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node* const node, int lvl) {
    processUnaryExpression(node, lvl, true);
}

void til::type_checker::do_not_node(cdk::not_node* const node, int lvl) {
    processUnaryExpression(node, lvl, false);
}

//---------------------------------------------------------------------------
//--- Arithmetic expressions ------------------------------------------------

void til::type_checker::processLeftIntArithmeticExpression(
    cdk::binary_operation_node* const node,
    int lvl,
    bool validForDoubles,
    bool validForPointer
) {
    node->right()->accept(this, lvl + 2);

    // If the right child is an int, we make the node an int. Same for doubles.
    if (node->right()->is_typed(cdk::TYPE_INT) || (validForDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
        node->type(node->right()->type());
        return;
    }

    // If node->right is unspecified, since the left is either int or unspec, we make them both an int.
    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        return;
    }

    // If the right child is a pointer, we make the node a pointer.
    if (validForPointer && node->right()->is_typed(cdk::TYPE_POINTER)) {
        node->type(node->right()->type());

        if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
            node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        }
        return;
    }

    throw std::string("wrong type in right argument of arithmetic expression");
}

void til::type_checker::processLeftDoubleArithmeticExpression(
    cdk::binary_operation_node* const node,
    int lvl
) {
    node->right()->accept(this, lvl + 2);

    // If right child is int or double, the node will be a double
    if (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        return;
    }

    // If node->right is unspecified, we make it a double.
    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        return;
    }

    throw std::string("wrong type in right argument of arithmetic expression");
}

void til::type_checker::processLeftPointerArithmeticExpression(
    cdk::binary_operation_node* const node,
    int lvl,
    const std::pair<bool, bool>& validForPointers
) {
    node->right()->accept(this, lvl + 2);

    // If right child is int, like before, the node will be a pointer.
    if (node->right()->is_typed(cdk::TYPE_INT)) {
        node->type(node->left()->type());
        return;
    }

    // If node->right is unspecified, we make it an int but make the node a pointer.
    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        node->type(node->left()->type());
        return;
    }

    // If both pointers refer to the same type, node will be an integer.
    if (validForPointers.second && compareTypes(node->left()->type(), node->right()->type(), false)) {
        node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        return;
    }

    throw std::string("wrong type in right argument of arithmetic expression");
}

/**
 * @brief type checks nodes for arithmetic expressions
 *
 * @param node
 * @param lvl
 * @param validForDoubles can a child with double type exist
 * @param validForPointers first -> one child can be a pointer | second -> both children can be pointers
 */
void til::type_checker::processArithmeticExpression(
    cdk::binary_operation_node* const node,
    int lvl,
    bool validForDoubles,
    const std::pair<bool, bool>& validForPointers
) {
    ASSERT_UNSPEC;

    node->left()->accept(this, lvl + 2);

    // left is int or unspec
    if (node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        processLeftIntArithmeticExpression(node, lvl, validForDoubles, validForPointers.first);

        if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
            node->left()->type(node->type());
        }
    }
    // left is double (if allowed)
    else if (validForDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
        processLeftDoubleArithmeticExpression(node, lvl);
    }
    // left is a pointer (if allowed)
    else if (validForPointers.first && node->left()->is_typed(cdk::TYPE_POINTER)) {
        processLeftPointerArithmeticExpression(node, lvl, validForPointers);
    }
}

void til::type_checker::do_add_node(cdk::add_node* const node, int lvl) {
    processArithmeticExpression(node, lvl, true, std::make_pair(true, false));
}

void til::type_checker::do_sub_node(cdk::sub_node* const node, int lvl) {
    processArithmeticExpression(node, lvl, true, std::make_pair(true, true));
}

void til::type_checker::do_mul_node(cdk::mul_node* const node, int lvl) {
    processArithmeticExpression(node, lvl, true, std::make_pair(false, false));
}

void til::type_checker::do_div_node(cdk::div_node* const node, int lvl) {
    processArithmeticExpression(node, lvl, true, std::make_pair(false, false));
}

void til::type_checker::do_mod_node(cdk::mod_node* const node, int lvl) {
    processArithmeticExpression(node, lvl, false, std::make_pair(false, false));
}

//---------------------------------------------------------------------------
//--- Comparative/logical expressions ---------------------------------------

void til::type_checker::processLeftIntComparativeExpression(
    cdk::binary_operation_node* const node,
    int lvl,
    bool validForDoubles,
    bool validForPointers
) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->type(node->left()->type());
    }
    else if (!node->right()->is_typed(cdk::TYPE_INT)
        && !(validForDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))
        && !(validForPointers && node->right()->is_typed(cdk::TYPE_POINTER))) {
        throw std::string("wrong type in right argument of binary expression");
    }
}

void til::type_checker::processLeftDoubleComparativeExpression(
    cdk::binary_operation_node* const node,
    int lvl
) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->type(node->left()->type());
    }
    else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
        throw std::string("wrong type in right argument of binary expression");
    }
}

void til::type_checker::processLeftPointerComparativeExpression(
    cdk::binary_operation_node* const node,
    int lvl
) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_POINTER)) {
        throw std::string("wrong type in right argument of binary expression");
    }
}

/**
 * @brief type checks nodes for comparative and logical expressions
 *
 * @param node
 * @param lvl
 * @param validForDoubles can a child with double type exist
 * @param validForPointers can a child with pointer type exist
 */
void til::type_checker::processComparativeExpression(
    cdk::binary_operation_node* const node,
    int lvl,
    bool validForDoubles,
    bool validForPointers
) {
    ASSERT_UNSPEC;
    node->left()->accept(this, lvl + 2);

    if (node->left()->is_typed(cdk::TYPE_INT)) {
        processLeftIntComparativeExpression(node, lvl, validForDoubles, validForPointers);
    }
    else if (validForDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
        processLeftDoubleComparativeExpression(node, lvl);
    }
    else if (validForPointers && node->left()->is_typed(cdk::TYPE_POINTER)) {
        processLeftPointerComparativeExpression(node, lvl);
    }
    else if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->right()->accept(this, lvl + 2);

        if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
            node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
            node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        }
        else if (node->right()->is_typed(cdk::TYPE_INT) || (validForDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
            node->left()->type(node->right()->type());
        }
        else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
            node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        }
        else {
            throw std::string("wrong type in right argument of binary expression");
        }
    }
    else {
        throw std::string("wrong type in left argument of binary expression");
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_lt_node(cdk::lt_node* const node, int lvl) {
    processComparativeExpression(node, lvl, true, false);
}

void til::type_checker::do_le_node(cdk::le_node* const node, int lvl) {
    processComparativeExpression(node, lvl, true, false);
}

void til::type_checker::do_ge_node(cdk::ge_node* const node, int lvl) {
    processComparativeExpression(node, lvl, true, false);
}

void til::type_checker::do_gt_node(cdk::gt_node* const node, int lvl) {
    processComparativeExpression(node, lvl, true, false);
}

void til::type_checker::do_ne_node(cdk::ne_node* const node, int lvl) {
    processComparativeExpression(node, lvl, true, true);
}

void til::type_checker::do_eq_node(cdk::eq_node* const node, int lvl) {
    processComparativeExpression(node, lvl, true, true);
}

void til::type_checker::do_and_node(cdk::and_node* const node, int lvl) {
    processComparativeExpression(node, lvl, false, false);
}

void til::type_checker::do_or_node(cdk::or_node* const node, int lvl) {
    processComparativeExpression(node, lvl, false, false);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node* const node, int lvl) {
    ASSERT_UNSPEC;
    const std::string& id = node->name();
    std::shared_ptr<til::symbol> symbol = _symtab.find(id);

    if (symbol != nullptr) {
        node->type(symbol->type());
    }
    else {
        throw id;
    }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node* const node, int lvl) {
    ASSERT_UNSPEC;
    try {
        node->lvalue()->accept(this, lvl);
        node->type(node->lvalue()->type());
    }
    catch (const std::string& id) {
        throw "undeclared variable '" + id + "'";
    }
}

void til::type_checker::do_assignment_node(cdk::assignment_node* const node, int lvl) {
    ASSERT_UNSPEC;

    node->lvalue()->accept(this, lvl);
    node->rvalue()->accept(this, lvl);

    if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
        node->rvalue()->type(node->lvalue()->type());
    }
    else if (node->rvalue()->is_typed(cdk::TYPE_POINTER) && node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
        auto lref = cdk::reference_type::cast(node->lvalue()->type());
        auto rref = cdk::reference_type::cast(node->rvalue()->type());

        if (rref->referenced()->name() == cdk::TYPE_UNSPEC
            || rref->referenced()->name() == cdk::TYPE_VOID
            || lref->referenced()->name() == cdk::TYPE_VOID) {
            node->rvalue()->type(node->lvalue()->type());
        }
    }

    if (!compareTypes(node->lvalue()->type(), node->rvalue()->type(), true)) {
        throw std::string("wrong type in right argument of assignment expression");
    }

    node->type(node->lvalue()->type());
}

//---------------------------------------------------------------------------
void til::type_checker::do_evaluation_node(til::evaluation_node* const node, int lvl) {
    node->argument()->accept(this, lvl);

    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (node->argument()->is_typed(cdk::TYPE_POINTER)) {
        std::shared_ptr<cdk::reference_type> pointed = cdk::reference_type::cast(node->argument()->type());

        if (pointed != nullptr && pointed->referenced()->name() == cdk::TYPE_UNSPEC) {
            node->argument()->type(cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
        }
    }
}

void til::type_checker::do_print_node(til::print_node* const node, int lvl) {
    for (size_t i = 0; i < node->arguments()->size(); i++) {
        auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));

        child->accept(this, lvl);

        if (child->is_typed(cdk::TYPE_UNSPEC)) {
            child->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        }
        else if (!child->is_typed(cdk::TYPE_INT) && !child->is_typed(cdk::TYPE_DOUBLE)
            && !child->is_typed(cdk::TYPE_STRING)) {
            throw std::string("wrong type for argument " + std::to_string(i) + " of print instruction");
        }
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node* const node, int lvl) {
    ASSERT_UNSPEC;

    node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_while_node(til::while_node* const node, int lvl) {
    node->condition()->accept(this, lvl + 4);

    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of loop instruction");
    }
}

void til::type_checker::do_stop_node(til::stop_node* const node, int lvl) {
    // EMPTY
}

void til::type_checker::do_next_node(til::next_node* const node, int lvl) {
    // EMPTY
}

void til::type_checker::do_return_node(til::return_node* const node, int lvl) {
    // symbol of current function is stored in the previous context
    auto symbol = _symtab.find("@", 1);
    if (symbol == nullptr) {
        throw std::string("return statement outside program block");
    }

    std::shared_ptr<cdk::functional_type> func_t = cdk::functional_type::cast(symbol->type());

    auto ret_t = func_t->output(0);
    auto rettype_name = ret_t->name();

    if (node->retval() == nullptr) {
        if (rettype_name != cdk::TYPE_VOID) {
            throw std::string("no return value specified for non-void function");
        }
        return;
    }

    // return has expression

    if (rettype_name == cdk::TYPE_VOID) {
        throw std::string("return value specified for void function");
    }

    node->retval()->accept(this, lvl + 2);

    if (!compareTypes(ret_t, node->retval()->type(), true)) {
        throw std::string("wrong type for return expression");
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_block_node(til::block_node* const node, int lvl) {
    // EMPTY
}

void til::type_checker::do_if_node(til::if_node* const node, int lvl) {
    node->condition()->accept(this, lvl + 4);

    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of if instruction");
    }
}

void til::type_checker::do_if_else_node(til::if_else_node* const node, int lvl) {
    node->condition()->accept(this, lvl + 4);

    if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
        node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in condition of if instruction");
    }
}

//---------------------------------------------------------------------------

void til::type_checker::do_sizeof_node(til::sizeof_node* const node, int lvl) {
    ASSERT_UNSPEC;
    node->expression()->accept(this, lvl + 2);

    if (node->expression()->is_typed(cdk::TYPE_UNSPEC)) {
        node->expression()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }

    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_address_of_node(til::address_of_node* const node, int lvl) {
    ASSERT_UNSPEC;

    node->lvalue()->accept(this, lvl + 2);
    if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
        auto ref = cdk::reference_type::cast(node->lvalue()->type());
        if (ref->referenced()->name() == cdk::TYPE_VOID) {
            // [[void]] is the same as [void]
            node->type(node->lvalue()->type());
            return;
        }
    }
    node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void til::type_checker::do_index_node(til::index_node* const node, int lvl) {
    ASSERT_UNSPEC;

    node->base()->accept(this, lvl + 2);
    if (!node->base()->is_typed(cdk::TYPE_POINTER)) {
        throw std::string("wrong type in pointer index's base (expected pointer)");
    }

    node->index()->accept(this, lvl + 2);
    if (node->index()->is_typed(cdk::TYPE_UNSPEC)) {
        node->index()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->index()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in pointer index's index (expected integer)");
    }

    auto basetype = cdk::reference_type::cast(node->base()->type());

    if (basetype->referenced()->name() == cdk::TYPE_UNSPEC) {
        basetype = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT));
        node->base()->type(basetype);
    }

    node->type(basetype->referenced());
}

void til::type_checker::do_alloc_node(til::alloc_node* const node, int lvl) {
    ASSERT_UNSPEC;

    node->argument()->accept(this, lvl + 2);

    if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
        node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    }
    else if (!node->argument()->is_typed(cdk::TYPE_INT)) {
        throw std::string("wrong type in argument of unary expression");
    }

    node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

void til::type_checker::do_null_node(til::null_node* const node, int lvl) {
    ASSERT_UNSPEC;

    node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_declaration_node(til::variable_declaration_node* const node, int lvl) {
    if (node->type() == nullptr) { // auto
        node->initializer()->accept(this, lvl + 2);

        if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
            node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        }
        else if (node->initializer()->is_typed(cdk::TYPE_POINTER)) {
            auto ref = cdk::reference_type::cast(node->initializer()->type());
            if (ref->referenced()->name() == cdk::TYPE_UNSPEC) {
                node->initializer()->type(cdk::reference_type::create(4,
                    cdk::primitive_type::create(4, cdk::TYPE_INT)));
            }
        }
        else if (node->initializer()->is_typed(cdk::TYPE_VOID)) {
            throw std::string("cannot declare variable of type void");
        }

        node->type(node->initializer()->type());
    }
    else { // not auto; node already has a type set
        if (node->initializer() != nullptr) {
            node->initializer()->accept(this, lvl + 2);

            if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
                if (node->is_typed(cdk::TYPE_DOUBLE)) {
                    node->initializer()->type(node->type());
                }
                else {
                    // if node->type() is not an int, a type mismatch error will be thrown later
                    node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
                }
            }
            else if (node->initializer()->is_typed(cdk::TYPE_POINTER) && node->is_typed(cdk::TYPE_POINTER)) {
                auto noderef = cdk::reference_type::cast(node->type());
                auto initref = cdk::reference_type::cast(node->initializer()->type());
                if (initref->referenced()->name() == cdk::TYPE_UNSPEC
                    || initref->referenced()->name() == cdk::TYPE_VOID
                    || noderef->referenced()->name() == cdk::TYPE_VOID) {
                    node->initializer()->type(node->type());
                }
            }

            if (!compareTypes(node->type(), node->initializer()->type(), true)) {
                throw std::string("wrong type in initializer for variable '" + node->identifier() + "'");
            }
        }
    }

    if (node->qualifier() == tEXTERNAL && !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
        throw std::string("foreign declaration of non-function '" + node->identifier() + "'");
    }

    auto symbol = make_symbol(node->identifier(), node->type(), node->qualifier());

    if (_symtab.insert(node->identifier(), symbol)) {
        _parent->set_new_symbol(symbol);
        return;
    }

    auto prev = _symtab.find(node->identifier());

    if (prev != nullptr && prev->qualifier() == tFORWARD) {
        if (compareTypes(prev->type(), symbol->type(), false)) {
            _symtab.replace(node->identifier(), symbol);
            _parent->set_new_symbol(symbol);
            return;
        }
    }

    throw std::string("redeclaration of variable '" + node->identifier() + "'");
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_node(til::function_node* const node, int lvl) {

    auto function = til::make_symbol("@", node->type());
    function->is_main(node->is_main());

    if (!_symtab.insert(function->name(), function)) {
        // if it can't insert, it's because it already exists in local context
        _symtab.replace(function->name(), function);
    }
}

void til::type_checker::do_function_call_node(til::function_call_node* const node, int lvl) {
    ASSERT_UNSPEC;

    std::shared_ptr<cdk::functional_type> functype;

    if (node->func() == nullptr) { // recursive call; "@"
        auto symbol = _symtab.find("@", 1);
        if (symbol == nullptr) {
            throw std::string("recursive call outside function");
        }
        else if (symbol->is_main()) {
            throw std::string("recursive call inside begin end block");
        }

        functype = cdk::functional_type::cast(symbol->type());
    }
    else {
        node->func()->accept(this, lvl);

        if (!node->func()->is_typed(cdk::TYPE_FUNCTIONAL)) {
            throw std::string("wrong type in function call");
        }

        functype = cdk::functional_type::cast(node->func()->type());
    }

    if (functype->input()->length() != node->arguments()->size()) {
        throw std::string("wrong number of arguments in function call");
    }

    for (size_t i = 0; i < node->arguments()->size(); i++) {
        auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));
        arg->accept(this, lvl);

        auto paramtype = functype->input(i);

        if (arg->is_typed(cdk::TYPE_UNSPEC)) {
            if (paramtype->name() == cdk::TYPE_DOUBLE) {
                arg->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
            }
            else {
                arg->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
            }
        }
        else if (arg->is_typed(cdk::TYPE_POINTER) && paramtype->name() == cdk::TYPE_POINTER) {
            auto paramref = cdk::reference_type::cast(paramtype);
            auto argref = cdk::reference_type::cast(arg->type());

            if (argref->referenced()->name() == cdk::TYPE_UNSPEC
                || argref->referenced()->name() == cdk::TYPE_VOID
                || paramref->referenced()->name() == cdk::TYPE_VOID) {
                arg->type(paramtype);
            }
        }

        if (!compareTypes(paramtype, arg->type(), true)) {
            throw std::string("wrong type for argument " + std::to_string(i + 1) + " in function call");
        }
    }

    // note this may result in this node being typed TYPE_VOID
    node->type(functype->output(0));
}