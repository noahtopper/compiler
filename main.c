#include <stdio.h>

#include "ast.h"

#include "ast_printer.h"

#include "parser.tab.h"

extern FILE *yyout;  // the output of flex

extern T_prog program_ast;  // the output of bison

int main(int argc, char **argv) {
  /* while (1) { */
  /*   yylex(); */
  /* } */

  // print input program and parse errors to stderr
  yyout = stderr;

  // kick off the parser, which will store the result in program_ast
  yyparse();

  // print the ast
  print_prog(program_ast, 0);
}
