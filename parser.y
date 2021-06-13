/*
 * SimpleC Language Grammar
 *
 * Adapted from Jeff Lee's 1985 ANSI C grammar and Jutta Degener's
 * update from 1995.
 */

%{

#include <stdio.h>

#include "ast.h"
#include "ast_printer.h"

// the resulting AST
T_prog program_ast;

// lexer variables
extern char yytext[];
extern int column;
extern int yylineno;

extern int yylex(); // from flex
void yyerror(char *);  

%}

%token <strval> IDENTIFIER
%token <intval> HEX_CONSTANT INT_CONSTANT
%token <charval> CHAR_CONSTANT
%token <strval> STRING_LITERAL
%token LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP
%token CHAR INT ARRAY POINTER FUNCTION ARROW
%token IF ELSE WHILE RETURN
%token MAIN

%start program

%left '-' '+'
%left '*' '/' '%'
%left '<' '>' LE_OP GE_OP
%left EQ_OP NE_OP
%left AND_OP OR_OP

/* Handle the dangling-else problem */
%expect 1

%union value {
  string strval;
  int intval;
  char charval;
  T_prog prog;
  T_expr expr;
  T_exprlist exprlist;
  E_op op;
  T_stmt stmt;
  T_stmtlist stmtlist;
  T_type type;
  T_typelist typelist;
  T_decl decl;
  T_decllist decllist;
  T_paramlist paramlist;
  T_func func;
  T_funclist funclist;
  T_main main;
}

%nterm <prog>        program
%nterm <expr>        primary_expression
%nterm <expr>        postfix_expression
%nterm <exprlist>    argument_list_opt
%nterm <exprlist>    argument_list
%nterm <expr>        unary_expression
%nterm <op>          unary_operator
%nterm <expr>        expression
%nterm <op>          binary_operator
%nterm <stmt>        assignment_statement
%nterm <stmt>        if_statement
%nterm <stmt>        while_statement
%nterm <stmt>        compound_statement
%nterm <stmt>        statement
%nterm <stmtlist>    statement_list
%nterm <type>        type
%nterm <typelist>    type_list_opt
%nterm <typelist>    type_list
%nterm <decl>        declaration
%nterm <decllist>    declaration_list
%nterm <paramlist>   parameter_list_opt
%nterm <paramlist>   parameter_list
%nterm <func>        function
%nterm <funclist>    function_list
%nterm <main>        main_function

%%

/* starting symbol */

program
  : declaration_list function_list main_function
  {
    $$ = create_prog($1, $2, $3);
    program_ast = $$;
  }
  ;

/* expressions */

// primary_expression -> IDENTIFIER
// primary_expression -> IDENTIFIER '(' argument_list_opt ')'
primary_expression 
  : IDENTIFIER { /* todo */ }
  | IDENTIFIER '(' argument_list_opt ')' { /* todo */ }
  | HEX_CONSTANT { /* todo */ }
  | INT_CONSTANT { $$ = create_intexpr($1); }
  | CHAR_CONSTANT { /* todo */ }
  | STRING_LITERAL { /* todo */ }
  | '(' expression ')' { /* todo */ }
  ;

postfix_expression
  : primary_expression { $$ = $1; }
  | postfix_expression '[' expression ']' { /* todo */ }
  ;

argument_list_opt
  : /* empty */ { /* todo */ }
  | argument_list { /* todo */ }
  ;

argument_list
  : expression { /* todo */ }
  | expression ',' argument_list { /* todo */ }
  ;

unary_expression
  : postfix_expression { $$ = $1; }
  | unary_operator unary_expression { /* todo */ }
  | '(' type ')' unary_expression { /* todo */ }
  ;

unary_operator
  : '&' { /* todo */ }
  | '*' { /* todo */ }
  | '-' { /* todo */ }
  | '!' { /* todo */ }
  ;

expression
  : unary_expression { /* todo */ }
  | expression binary_operator unary_expression { /* todo */ }
  ;

binary_operator
  : '*' { /* todo */ }
  | '/' { /* todo */ }
  | '%' { /* todo */ }
  | '+' { /* todo */ }
  | '-' { /* todo */ }
  | '<' { /* todo */ }
  | '>' { /* todo */ }
  | LE_OP  { /* todo */ }
  | GE_OP  { /* todo */ }
  | EQ_OP { /* todo */ }
  | NE_OP { /* todo */ }
  | AND_OP { /* todo */ }
  | OR_OP { /* todo */ }
  ;

/* statements */

assignment_statement
  : expression '=' expression ';' { /* todo */ }
  ;

if_statement
  : IF '(' expression ')' statement { /* todo */ }
  | IF '(' expression ')' statement ELSE statement { /* todo */ }
  ;

while_statement
  : WHILE '(' expression ')' statement { /* todo */ }
  ;

// compound_statement -> declaration_list statement_list
compound_statement
  : '{' declaration_list statement_list '}' { /* todo */ }
  ;

statement
  : assignment_statement { /* todo */ }
  | if_statement { /* todo */ }
  | while_statement { /* todo */ }
  | compound_statement { /* todo */ }
  ;

// statement_list -> statement statement_list
statement_list
  : /* empty */  { $$ = NULL; }
  | statement statement_list { $$ = create_stmtlist($1, $2); }
  ;

/* types */

type
  : INT { $$ = create_primitivetype(E_typename_int); }
  | CHAR { /* todo */ }
  | POINTER '<' type '>' { /* todo */ }
  | ARRAY '<' INT_CONSTANT ',' type '>' { /* todo */ }
  | FUNCTION '(' type_list_opt ')' ARROW type { /* todo */ }
  ;

type_list_opt
  : /* empty */  { /* todo */ }
  | type_list { /* todo */ }
  ;

type_list
  : type { /* todo */ }
  | type ',' type_list { /* todo */ }
  ;

/* declarations */

declaration
  : type IDENTIFIER ';' { $$ = create_decl($1, $2); }
  ;

declaration_list
  : /* empty */ { $$ = NULL; }
  | declaration declaration_list { $$ = create_decllist($1, $2); }
  ;

/* function definitions */

parameter_list_opt
  : /* empty */ { /* todo */ }
  | parameter_list { /* todo */ }
  ;

parameter_list
  : IDENTIFIER { /* todo */ }
  | IDENTIFIER ',' parameter_list { /* todo */ }
  ;

function
  : IDENTIFIER '(' parameter_list_opt ')' ':' type '{' declaration_list statement_list RETURN expression ';' '}'
  {
    /* todo */
  }
  ;

function_list
  : /* empty */ { $$ = NULL; }
  | function function_list { $$ = create_funclist($1, $2); }
  ;

// main is separated from other functions so that complete programs
// can be compiled, linked, and run before having to support functions
main_function
  : MAIN '{' declaration_list statement_list RETURN expression ';' '}'
  {
    $$ = create_main($3, $4, $6);
  }
  ;

%%

void yyerror(char *s) {
  fflush(stderr);
  fprintf(stderr, "\n%*s\n%*s:%d:%d\n", column, "^", column, s, yylineno, column);
  /* fprintf(stderr, "%s:%d:%d\n", s, yylineno, column); */
  exit(1);
}
