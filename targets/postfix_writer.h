#ifndef __til_TARGETS_POSTFIX_WRITER_H__
#define __til_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <optional>
#include <set>
#include <sstream>
#include <stack>
#include <cdk/emitters/basic_postfix_emitter.h>
#include <cdk/types/types.h>

namespace til {

    //!
    //! Traverse syntax tree and generate the corresponding assembly code.
    //!
    class postfix_writer : public basic_ast_visitor {
        cdk::symbol_table<til::symbol>& _symtab;

        bool _forceGlobalFutureDecls = false;
        bool _inFunctionArgs = false;
        std::stack<std::string> _functionLabels; // stack of visited function labels
        std::string _currentFunctionRetLabel; // where to jump when a return occurs
        int _offset; // current frame pointer offset (0 means no vars defined)
        std::optional<std::string> _externalFunctionName;
        std::set<std::string> _externalFunctionsToDeclare;
        // (condition, end) labels for the loops in the current function context
        std::vector<std::pair<std::string, std::string>>* _currentFunctionLoops;
        bool _visitedFinalInstruction = false;

        cdk::basic_postfix_emitter& _pf;
        int _lbl;

    public:
        postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol>& symtab,
            cdk::basic_postfix_emitter& pf) :
            basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {}

    public:
        ~postfix_writer() {
            os().flush();
        }

    protected:
        void convertBinaryExpressionIntDoubleArgs(cdk::binary_operation_node* const node, int lvl);
        void convertCovariantNode(std::shared_ptr<cdk::basic_type> const& target_type, cdk::expression_node* const node, int lvl);
        template<size_t P, typename T> void executeLoopControlInstruction(T* const node);

    private:
        /** Method used to generate sequential labels. */
        inline std::string mklbl(int lbl) {
            std::ostringstream oss;
            if (lbl < 0)
                oss << ".L" << -lbl;
            else
                oss << "_L" << lbl;
            return oss.str();
        }

        inline bool inFunction() {
            return !_forceGlobalFutureDecls && !_functionLabels.empty();
        }

        template<class T>
        inline bool instanceOf(cdk::basic_node* const node) {
            return dynamic_cast<T*>(node) != nullptr;
        }

    public:
        // do not edit these lines
#define __IN_VISITOR_HEADER__

#include ".auto/visitor_decls.h"       // automatically generated

#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

    };

#define THROW_ERROR_FOR_NODE(context, msg) { \
  std::cerr << context->lineno() << ": " << msg << std::endl; \
  return; \
}
#define THROW_ERROR(msg) THROW_ERROR_FOR_NODE(node, msg)

} // til

#endif
