#ifndef __TIL_AST_FUNCTION_DECLARATION_H__
#define __TIL_AST_FUNCTION_DECLARATION_H__

#include <string>
#include <memory>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/typed_node.h>

namespace til {

  class function_definition_node : public cdk::typed_node {
    std::string _identifier;
    cdk::sequence_node *_parameters;
    int _qualifier;

  public:
    function_definition_node(int lineno, int qualifier, const std::string &identifier, cdk::sequence_node *parameters) :
      cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _parameters(parameters) {
        type(cdk::primitive_type::create(0, cdk::TYPE_VOID));
    }

    const std::string& identifier() const {
      return _identifier;
    }

    cdk::sequence_node* parameters() const {
      return _parameters;
    }

    int qualifier() const {
      return _qualifier;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }
  };

} // til

#endif
