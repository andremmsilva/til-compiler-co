#ifndef __TIL_AST_FUNCTION_DECLARATION_H__
#define __TIL_AST_FUNCTION_DECLARATION_H__

#include <string>
#include <memory>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/typed_node.h>

namespace til {

  class function_declaration_node : public cdk::typed_node {
    int _qualifier;
    std::string _identifier;
    cdk::sequence_node *_parameters;
    til::block_node *_body;

  public:
    function_declaration_node(int lineno, int qualifier, const std::string &identifier, cdk::sequence_node *parameters, til::block_node *body) :
      cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _parameters(parameters), _body(body) {
        type(cdk::primitive_type::create(0, cdk::TYPE_VOID));
    }

    const std::string& identifier() const {
      return _identifier;
    }

    cdk::sequence_node* parameters() const {
      return _parameters;
    }

    til::block_node* body() const {
      return _body;
    }

    int qualifier() const {
      return _qualifier;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_declaration_node(this, level);
    }
  };
}

#endif
