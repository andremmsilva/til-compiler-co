#ifndef __TIL_AST_WITH_H__
#define __TIL_AST_WITH_H__

namespace til
{

    class with_node : public cdk::basic_node
    {
        cdk::expression_node *_func;
        cdk::expression_node *_vector;
        cdk::expression_node *_low;
        cdk::expression_node *_high;

    public:
        with_node(int lineno, cdk::expression_node *func, cdk::expression_node *vector, cdk::expression_node *low, cdk::expression_node *high) : cdk::basic_node(lineno), _func(func), _vector(vector), _low(low), _high(high) {}

    public:
        inline cdk::expression_node *func()
        {
            return _func;
        }
        inline cdk::expression_node *vector()
        {
            return _vector;
        }
        inline cdk::expression_node *low()
        {
            return _low;
        }
        inline cdk::expression_node *high()
        {
            return _high;
        }

        void accept(basic_ast_visitor *sp, int level)
        {
            sp->do_with_node(this, level);
        }
    };

}

#endif