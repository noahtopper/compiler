#include "ast.h"
#include "ast_printer.h"

#include "ast.c"

int main() {
  T_stmt stmt
    = create_assignstmt(
                        create_identexpr("a"),
                        create_binaryexpr(create_identexpr("c"),
                                          E_op_plus,
                                          create_intexpr(7)));
  print_stmt(stmt, 0);
  T_stmtlist stmtlist = create_stmtlist(stmt, NULL);
}
