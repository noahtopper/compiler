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
  : IDENTIFIER { $$ = create_identexpr($1); }
  | IDENTIFIER '(' argument_list_opt ')' { $$ = create_callexpr($1, $3); }
  | HEX_CONSTANT { $$ = create_intexpr($1); }
  | INT_CONSTANT { $$ = create_intexpr($1); }
  | CHAR_CONSTANT { $$ = create_charexpr($1); }
  | STRING_LITERAL { $$ = create_strexpr($1); }
  | '(' expression ')' { $$ = $2; }
  ;

postfix_expression
  : primary_expression { $$ = $1; }
  | postfix_expression '[' expression ']' { $$ = create_arrayexpr($1, $3); }
  ;

argument_list_opt
  : /* empty */ { $$ = NULL; }
  | argument_list { $$ = $1; }
  ;

argument_list
  : expression { $$ = create_exprlist($1, NULL); }
  | expression ',' argument_list { $$ = create_exprlist($1, $3); }
  ;

unary_expression
  : postfix_expression { $$ = $1; }
  | unary_operator unary_expression { $$ = create_unaryexpr($1, $2); }
  | '(' type ')' unary_expression { $$ = create_castexpr($2, $4); }
  ;

unary_operator
  : '&' { $$ = E_op_ref; }
  | '*' { $$ = E_op_deref; }
  | '-' { $$ = E_op_minus; }
  | '!' { $$ = E_op_not; }
  ;

expression
  : unary_expression { $$ = $1; }
  | expression binary_operator unary_expression { $$ = create_binaryexpr($1, $2, $3); }
  ;

binary_operator
  : '*' { $$ = E_op_times; }
  | '/' { $$ = E_op_divide; }
  | '%' { $$ = E_op_mod; }
  | '+' { $$ = E_op_plus; }
  | '-' { $$ = E_op_minus; }
  | '<' { $$ = E_op_lt; }
  | '>' { $$ = E_op_gt; }
  | LE_OP { $$ = E_op_le; }
  | GE_OP { $$ = E_op_ge; }
  | EQ_OP { $$ = E_op_eq; }
  | NE_OP { $$ = E_op_ne; }
  | AND_OP { $$ = E_op_and; }
  | OR_OP { $$ = E_op_or; }
  ;

/* statements */

assignment_statement
  : expression '=' expression ';' { $$ = create_assignstmt($1, $3); }
  ;

if_statement
  : IF '(' expression ')' statement { $$ = create_ifstmt($3, $5); }
  | IF '(' expression ')' statement ELSE statement { $$ = create_ifelsestmt($3, $5, $7); }
  ;

while_statement
  : WHILE '(' expression ')' statement { $$ = create_whilestmt($3, $5); }
  ;

// compound_statement -> declaration_list statement_list
compound_statement
  : '{' declaration_list statement_list '}' { $$ = create_compoundstmt($2, $3); }
  ;

statement
  : assignment_statement { $$ = $1; }
  | if_statement { $$ = $1; }
  | while_statement { $$ = $1; }
  | compound_statement { $$ = $1; }
  ;

// statement_list -> statement statement_list
statement_list
  : /* empty */  { $$ = NULL; }
  | statement statement_list { $$ = create_stmtlist($1, $2); }
  ;

/* types */

type
  : INT { $$ = create_primitivetype(E_typename_int); }
  | CHAR { $$ = create_primitivetype(E_typename_char); }
  | POINTER '<' type '>' { $$ = create_pointertype($3); }
  | ARRAY '<' INT_CONSTANT ',' type '>' { $$ = create_arraytype($3, $5); }
  | FUNCTION '(' type_list_opt ')' ARROW type { $$ = create_functiontype($3, $6); }
  ;

type_list_opt
  : /* empty */  { $$ = NULL; }
  | type_list { $$ = $1; }
  ;

type_list
  : type { $$ = create_typelist($1, NULL); }
  | type ',' type_list { $$ = create_typelist($1, $3); }
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
  : /* empty */ { $$ = NULL; }
  | parameter_list { $$ = $1; }
  ;

parameter_list
  : IDENTIFIER { $$ = create_paramlist($1, NULL); }
  | IDENTIFIER ',' parameter_list { $$ = create_paramlist($1, $3); }
  ;

function
  : IDENTIFIER '(' parameter_list_opt ')' ':' type '{' declaration_list statement_list RETURN expression ';' '}'
  {
    $$ = create_func($1, $3, $6, $8, $9, $11);
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
