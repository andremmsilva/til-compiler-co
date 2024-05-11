#ifndef __TIL_AST_PRINT_H__
#define __TIL_AST_PRINT_H__

#include <cdk/ast/sequence_node.h>

namespace til {

  class print_node: public cdk::basic_node {
    cdk::sequence_node *_arguments;
    bool _newline = false; // true -> println

  public:
    print_node(int lineno, cdk::sequence_node *arguments, bool newline = false) :
        cdk::basic_node(lineno), _arguments(arguments), _newline(newline) {
    }

  public:
    inline cdk::sequence_node* arguments() {
      return _arguments;
    }
    bool newline() {
      return _newline;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_print_node(this, level);
    }

  };

}

#endif