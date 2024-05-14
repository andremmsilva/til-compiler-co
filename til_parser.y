%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type>               type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                                            i;           /* integer value */
  double                                         d;           /* double value */
  std::string                                   *s;           /* symbol name or string literal */
  cdk::basic_node                               *node;        /* node pointer */
  cdk::sequence_node                            *sequence;
  cdk::expression_node                          *expression;  /* expression nodes */
  cdk::lvalue_node                              *lvalue;
  til::block_node                               *block;
  std::vector<std::shared_ptr<cdk::basic_type>> *type_vec;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTR
%token tINT_T tDOUBLE_T tSTRING_T tVOID_T tVAR
%token tEXTERNAL tFORWARD tPUBLIC tPRIVATE
%token tIF tLOOP tSTOP tNEXT tRETURN
%token tREAD tNULL tSIZEOF tOBJECTS tINDEX tSET
%token tPROGRAM tBLOCK tFUNCTION
%token tPRINT tPRINTLN

%nonassoc tIFX
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '(' 

%type <sequence> qualified_decls declarations instructions exprs function_args
%type <node> qualified_decl program declaration instruction if_instruction function_arg
%type <type> type referable_type func_return_type func_type void_ptr_type
%type <type_vec> types
%type <block> decls_instrs block
%type <expression> expr function_def
%type <lvalue> lval

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : qualified_decls program  { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); }
     | qualified_decls          { compiler->ast($1); }
     | program                  { compiler->ast(new cdk::sequence_node(LINE, $1)); }
     | /* empty */              { compiler->ast(new cdk::sequence_node(LINE)); }
     ;

qualified_decls : qualified_decls qualified_decl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
                | qualified_decl                    { $$ = new cdk::sequence_node(LINE, $1); }
                ;

qualified_decl : '(' tEXTERNAL type  tIDENTIFIER      ')'    { $$ = new til::variable_declaration_node(LINE, tEXTERNAL, $3, *$4, nullptr); delete $4; }
               | '(' tFORWARD  type  tIDENTIFIER      ')'    { $$ = new til::variable_declaration_node(LINE, tFORWARD, $3, *$4, nullptr); delete $4; }
               | '(' tPUBLIC   type  tIDENTIFIER      ')'    { $$ = new til::variable_declaration_node(LINE, tPUBLIC, $3, *$4, nullptr); delete $4; }
               | '(' tPUBLIC   type  tIDENTIFIER expr ')'    { $$ = new til::variable_declaration_node(LINE, tPUBLIC, $3, *$4, $5); delete $4; }
               | '(' tPUBLIC   tVAR  tIDENTIFIER expr ')'    { $$ = new til::variable_declaration_node(LINE, tPUBLIC, nullptr, *$4, $5); delete $4; }
               | '(' tPUBLIC         tIDENTIFIER expr ')'    { $$ = new til::variable_declaration_node(LINE, tPUBLIC, nullptr, *$3, $4); delete $3; }
               |  declaration                                { $$ = $1; }
               ;

declarations : declarations declaration    { $$ = new cdk::sequence_node(LINE, $2, $1); }
             | declaration                 { $$ = new cdk::sequence_node(LINE, $1); }
             ;

declaration : '(' type tIDENTIFIER      ')'    { $$ = new til::variable_declaration_node(LINE, tPRIVATE, $2, *$3, nullptr); delete $3; }
            | '(' type tIDENTIFIER expr ')'    { $$ = new til::variable_declaration_node(LINE, tPRIVATE, $2, *$3, $4); delete $3; }
            | '(' tVAR tIDENTIFIER expr ')'    { $$ = new til::variable_declaration_node(LINE, tPRIVATE, nullptr, *$3, $4); delete $3; }
            | '(' tIDENTIFIER expr      ')'    { $$ = new til::variable_declaration_node(LINE, tPRIVATE, nullptr, *$2, $3); delete $3; }
            ;

program : '(' tPROGRAM decls_instrs ')'    { $$ = new til::function_node(LINE, $3); }
        ;

decls_instrs : declarations instructions    { $$ = new til::block_node(LINE, $1, $2); }
             | declarations                 { $$ = new til::block_node(LINE, $1, new cdk::sequence_node(LINE)); }
             | instructions                 { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $1); }
             |                              { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
             ;

function_def : '(' tFUNCTION '(' func_return_type function_args ')' decls_instrs ')'   { $$ = new til::function_node(LINE, $5, $7, $4, false); }
             | '(' tFUNCTION '(' func_return_type ')' decls_instrs ')'                 { $$ = new til::function_node(LINE, new cdk::sequence_node(LINE), $6, $4, false); }
             ;

function_arg : '(' type tIDENTIFIER ')'  { $$ = new til::variable_declaration_node(LINE, tPRIVATE, $2, *$3, nullptr); delete $3; }
             ;

function_args : function_args function_arg    { $$ = new cdk::sequence_node(LINE, $2, $1); }
              | function_arg                  { $$ = new cdk::sequence_node(LINE, $1); }
              ;

types : types type    { $$ = $1; $$->push_back($2); }
      | type          { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(1, $1); }
      ;

type : referable_type    { $$ = $1; }
     | void_ptr_type     { $$ = $1; }
     ;

referable_type : tINT_T                 { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
               | tSTRING_T              { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
               | tDOUBLE_T              { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
               | func_type              { $$ = $1; }
               | referable_type '!'     { $$ = cdk::reference_type::create(4, $1); } /* ptr */
               ;

func_type : '(' func_return_type ')'                { $$ = cdk::functional_type::create($2); }
          | '(' func_return_type '(' types ')' ')'  { $$ = cdk::functional_type::create(*$4, $2); delete $4; }
          ;

func_return_type : type       { $$ = $1; }
                 | tVOID_T    { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
                 ;

void_ptr_type :  void_ptr_type '!'   { $$ = $1; }
              |  tVOID_T '!'         { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)); }
              ;

block : '(' tBLOCK decls_instrs ')'  { $$ = $3; }
      ;

instructions : instructions instruction  { $$ = new cdk::sequence_node(LINE, $2, $1); }
             | instruction               { $$ = new cdk::sequence_node(LINE, $1); }
             ;

instruction : expr                            { $$ = new til::evaluation_node(LINE, $1); }
            | '(' tPRINT exprs ')'            { $$ = new til::print_node(LINE, $3, false); }
            | '(' tPRINTLN exprs ')'          { $$ = new til::print_node(LINE, $3, true); }
            | '(' tSTOP tINTEGER ')'          { $$ = new til::stop_node(LINE, $3); }
            | '(' tSTOP ')'                   { $$ = new til::stop_node(LINE, 1); }
            | '(' tNEXT tINTEGER ')'          { $$ = new til::next_node(LINE, $3); }
            | '(' tNEXT ')'                   { $$ = new til::next_node(LINE, 1); }
            | '(' tRETURN expr ')'            { $$ = new til::return_node(LINE, $3); }
            | '(' tRETURN ')'                 { $$ = new til::return_node(LINE, nullptr); }
            | '(' tLOOP expr instruction ')'  { $$ = new til::while_node(LINE, $3, $4); }
            | if_instruction                  { $$ = $1; }
            | block                           { $$ = $1; }
            ;

if_instruction : '(' tIF expr instruction ')' /*%prec tIFX */   { $$ = new til::if_node(LINE, $3, $4); }
               | '(' tIF expr instruction instruction ')'  { $$ = new til::if_else_node(LINE, $3, $4, $5); }
               ;

exprs : exprs expr    { $$ = new cdk::sequence_node(LINE, $2, $1); }
      |       expr    { $$ = new cdk::sequence_node(LINE, $1); }
      ;

expr : tINTEGER                       { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLE                        { $$ = new cdk::double_node(LINE, $1); }
     | tSTR                           { $$ = new cdk::string_node(LINE, *$1); delete $1; }
     | tNULL                          { $$ = new til::null_node(LINE); }
     | '(' '+' expr ')' %prec tUNARY  { $$ = new cdk::unary_plus_node(LINE, $3); }
     | '(' '-' expr ')' %prec tUNARY  { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '~' expr ')'               { $$ = new cdk::not_node(LINE, $3); }
     | '(' '+' expr expr ')'          { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr expr ')'          { $$ = new cdk::sub_node(LINE, $3, $4); }
     | '(' '*' expr expr ')'          { $$ = new cdk::mul_node(LINE, $3, $4); }
     | '(' '/' expr expr ')'          { $$ = new cdk::div_node(LINE, $3, $4); }
     | '(' '%' expr expr ')'          { $$ = new cdk::mod_node(LINE, $3, $4); }
     | '(' '<' expr expr ')'          { $$ = new cdk::lt_node(LINE, $3, $4); }
     | '(' '>' expr expr ')'          { $$ = new cdk::gt_node(LINE, $3, $4); }
     | '(' tGE expr expr ')'          { $$ = new cdk::ge_node(LINE, $3, $4); }
     | '(' tLE expr expr ')'          { $$ = new cdk::le_node(LINE, $3, $4); }
     | '(' tNE expr expr ')'          { $$ = new cdk::ne_node(LINE, $3, $4); }
     | '(' tEQ expr expr ')'          { $$ = new cdk::eq_node(LINE, $3, $4); }
     | '(' tAND expr expr ')'         { $$ = new cdk::and_node(LINE, $3, $4); }
     | '(' tOR expr expr ')'          { $$ = new cdk::or_node(LINE, $3, $4); }
     | '(' tOBJECTS expr ')'          { $$ = new til::alloc_node(LINE, $3); }
     | '(' tSIZEOF expr ')'           { $$ = new til::sizeof_node(LINE, $3); }
     | lval                           { $$ = new cdk::rvalue_node(LINE, $1); }
     | '(' tSET lval expr ')'         { $$ = new cdk::assignment_node(LINE, $3, $4); }
     | '(' '?' lval ')'               { $$ = new til::address_of_node(LINE, $3); }
     | '(' tREAD ')'                  { $$ = new til::read_node(LINE); }
     | '(' expr exprs ')'             { $$ = new til::function_call_node(LINE, $2, $3); }
     | '(' expr ')'                   { $$ = new til::function_call_node(LINE, $2, new cdk::sequence_node(LINE)); }
     | '(' '@' exprs ')'              { $$ = new til::function_call_node(LINE, nullptr, $3); }
     | '(' '@' ')'                    { $$ = new til::function_call_node(LINE, nullptr, new cdk::sequence_node(LINE)); }
     | function_def                   { $$ = $1; }
     ;

lval : '(' tIDENTIFIER ')'            { $$ = new cdk::variable_node(LINE, $2); }
     | '(' tINDEX expr expr ')'       { $$ = new til::index_node(LINE, $3, $4); }
     ;


%%