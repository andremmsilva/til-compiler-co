%option c++ prefix="til_scanner_" outfile="til_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "til_parser.tab.h"

// don't change this
#define yyerror LexerError

#define SAFE_MAKE_INT(base) { \
  try { \
    yylval.i = std::stoi(yytext, nullptr, base); \
    return tINTEGER; \
  } catch (const std::out_of_range&) { \
    yyerror("overflow"); \
  } \
}

#define SAFE_MAKE_DOUBLE { \
  try { \
    yylval.d = std::stod(yytext); \
    return tDOUBLE; \
  } catch (const std::out_of_range&) { \
    yyerror("overflow"); \
  } \
}

%}

EXP [Ee][+-]?[0-9]+

%x X_COMMENT X_STRING X_IGNORE_STR

%%
  yydebug=1; set_debug(1);

";".*                 ; /* comentários "explicativos" */

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>.|\n        ; /* ignorar tudo dentro dos comments */

">="                   return tGE;
"<="                   return tLE;
"=="                   return tEQ;
"!="                   return tNE;
"&&"                   return tAND;
"||"                   return tOR;

"int"                  return tINT_T;
"double"               return tDOUBLE_T;
"string"               return tSTRING_T;
"void"                 return tVOID_T;

"external"             return tEXTERNAL;
"forward"              return tFORWARD;
"public"               return tPUBLIC;
"var"                  return tVAR;

"loop"                 return tLOOP;
"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;

"if"                   return tIF;

"read"                 return tREAD;
"null"                 return tNULL;
"sizeof"               return tSIZEOF;
"objects"              return tOBJECTS;
"index"                return tINDEX;
"set"                  return tSET;

"program"              return tPROGRAM;
"block"                return tBLOCK;
"function"             return tFUNCTION;

"print"                return tPRINT;
"println"              return tPRINTLN;

"with"                 return tWITH;

[A-Za-z][A-Za-z0-9]*   yylval.s = new std::string(yytext); return tIDENTIFIER;

\"                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"           yy_pop_state(); return tSTR;
<X_STRING>\\\"         *yylval.s += yytext + 1;
<X_STRING>\\\\         *yylval.s += yytext + 1;
<X_STRING>\\t          *yylval.s += '\t';
<X_STRING>\\n          *yylval.s += '\n';
<X_STRING>\\r          *yylval.s += '\r';
<X_STRING>\\0          yy_push_state(X_IGNORE_STR);
<X_STRING>\\[0-7]{1,3} {
                          int i = std::stoi(yytext + 1, nullptr, 8);
                          if (i > 255) yyerror("octal in string out of range");
                          *yylval.s += (char) i;
                       }
<X_STRING>\\.          *yylval.s += yytext + 1;
<X_STRING>\n           yyerror("newline in string");
<X_STRING>\0           yyerror("null byte in string");
<X_STRING>.            *yylval.s += yytext;

<X_IGNORE_STR>\"       yy_pop_state(); yy_pop_state(); return tSTR;
<X_IGNORE_STR>\\\"     ;
<X_IGNORE_STR>\\\\     ;
<X_IGNORE_STR>\n       yyerror("newline in string");
<X_IGNORE_STR>\0       yyerror("null byte in string");
<X_IGNORE_STR>.        ;

0|[1-9][0-9]*          SAFE_MAKE_INT(10);  /* base 10 integer */
0x[0-9a-fA-F]+         SAFE_MAKE_INT(16);  /* base 16 integer */

[0-9]*\.[0-9]+{EXP}?   SAFE_MAKE_DOUBLE;
[0-9]+\.[0-9]*{EXP}?   SAFE_MAKE_DOUBLE;
[0-9]+{EXP}            SAFE_MAKE_DOUBLE;

[-()<>=+*/%\[\]@?~!]   return *yytext;

[ \t\n\r]+             ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%