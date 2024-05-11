#ifndef __TIL_AST_FUNCTION_CALL_H__
#define __TIL_AST_FUNCTION_CALL_H__

#include <string>
#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/nil_node.h>
#include <cdk/ast/expression_node.h>

namespace til {

  //!
  //! Class for describing function call nodes.
  //!
  //! If _arguments is null, them the node is either a call to a function with
  //! no arguments (or in which none of the default arguments is present) or
  //! an access to a variable.
  //!
  class function_call_node : public cdk::expression_node {
    cdk::expression_node* _func;
    cdk::sequence_node* _arguments;

  public:
    /**
     * Constructor for a function call with arguments.
     */
    function_call_node(int lineno, cdk::expression_node* func, cdk::sequence_node* arguments) : cdk::expression_node(lineno), _func(func), _arguments(arguments) {}

  public:
    inline cdk::expression_node* func() {
      return _func;
    }
    inline cdk::sequence_node* arguments() {
      return _arguments;
    }

    void accept(basic_ast_visitor* sp, int level) {
      sp->do_function_call_node(this, level);
    }
  };
}

#endif