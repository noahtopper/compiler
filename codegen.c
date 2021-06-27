#include <stdio.h>

#include "codegen.h"
#include "table.h"

#define PUSH(arg1) printf( "\tpush\t%s\n", arg1 )
#define POP(arg1) printf( "\tpop\t%s\n", arg1 )
#define MOV(arg1, arg2) printf("\tmov\t%s, %s\n", arg1, arg2)
#define MOV_FROM_IMMEDIATE(arg1, arg2) printf("\tmov\t$%d, %s\n", arg1, arg2)
#define MOV_FROM_OFFSET(offset, reg) printf("\tmov\t-%d(%%rbp), %s\n", offset, reg)
#define MOV_TO_OFFSET(reg, offset) printf("\tmov\t%s, -%d(%%rbp)\n", reg, offset)
#define MOV_FROM_GLOBAL(reg, global) printf("\tmov\t%s(%%rip), %s\n", global, reg)
#define MOV_TO_GLOBAL(reg, global) printf("\tmov\t%s, %s(%%rip)\n", reg, global)
#define ADD(arg1, arg2) printf("\tadd\t%s, %s\n", arg1, arg2)
#define SUB(arg1, arg2) printf("\tsub\t%s, %s\n", arg1, arg2)
#define SUBCONST(arg1, arg2) printf("\tsub\t$%d, %s\n", arg1, arg2)
#define IMUL(arg1, arg2) printf("\timul\t%s, %s\n", arg1, arg2)
#define CDQ() printf("\tcdq\n")
#define IDIV(reg) printf("\tidiv\t%s\n", reg)
#define CALL(arg1) printf("\tcall\t%s\n", arg1)
#define RET printf("\tret\n")
#define COMMENT(arg1) printf("\t# %s\n", arg1)

const string const param_registers[] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"}; // all const

static void codegen_main(T_main main);

static void codegen_decllist(T_decllist decllist);

static void codegen_stmtlist(T_stmtlist stmtlist);

static void codegen_funclist(T_funclist funclist);

static void codegen_func(T_func func);

static void codegen_stmtlist(T_stmtlist stmtlist);

static void codegen_stmt(T_stmt stmt);

static void codegen_assignstmt(T_stmt stmt);

static void codegen_ifstmt(T_stmt stmt);

static void codegen_ifelsestmt(T_stmt stmt);

static void codegen_whilestmt(T_stmt stmt);

static void codegen_compoundstmt(T_stmt stmt);

static void codegen_expr(T_expr expr);

static void codegen_identexpr(T_expr expr);

static void codegen_callexpr(T_expr expr);

static void codegen_intexpr(T_expr expr);

static void codegen_charexpr(T_expr expr);

static void codegen_strexpr(T_expr expr);

static void codegen_arrayexpr(T_expr expr);

static void codegen_unaryexpr(T_expr expr);

static void codegen_binaryexpr(T_expr expr);

static void codegen_castexpr(T_expr expr);

static void emit_prologue(int size);

static void emit_epilogue();

static int bitwidth(T_type type);

typedef struct S_offset_scope *T_offset_scope;

struct S_offset_scope {
  T_table table;
  int stack_size;
  int current_offset;
  T_offset_scope parent;
};

static T_offset_scope create_offset_scope(T_offset_scope parent) {
  T_offset_scope offset_scope = xmalloc(sizeof(*offset_scope));
  offset_scope->table = create_table();
  // start after the dynamic link to the caller's %rbp
  offset_scope->current_offset = 8;
  offset_scope->stack_size = 0;
  offset_scope->parent = parent;
  return offset_scope;
}

static T_offset_scope destroy_offset_scope(T_offset_scope offset_scope) {
  T_offset_scope parent_offset_scope = offset_scope->parent;
  destroy_table(offset_scope->table);
  free(offset_scope);
  return parent_offset_scope;
}

struct S_offset {
  int offset;
};

typedef struct S_offset * T_offset;

static T_table global_symbols;

static T_offset_scope current_offset_scope;

static int label;

void insert_offset(T_offset_scope scope, string ident, int size) {
  T_offset entry = xmalloc(sizeof(*entry));
  entry->offset = scope->current_offset;
  insert(scope->table, ident, (void*) entry);
  scope->stack_size += size;
  scope->current_offset += size;
}

static int lookup_offset_in_scope(T_offset_scope offset_scope, string ident) {
  T_offset offset = (T_offset) lookup(offset_scope->table, ident);
  if (NULL == offset) {
    fprintf(stderr, "FATAL: symbol not in table.  double-check the code that inserts the symbol into the offset scope.");
  }
  return offset->offset;
}

static T_offset lookup_offset_in_all_offset_scopes(T_offset_scope offset_scope, string ident) {
  // loop over each offset_scope and check for a binding for ident
  while (NULL != offset_scope) {
    // check for a binding in this offset_scope
    T_offset offset = (T_offset) lookup(offset_scope->table, ident);
    if (NULL != offset) {
      return offset;
    }
    // no binding in this offset_scope, so check the parent
    offset_scope = offset_scope->parent;
  }
  // found no binding in any offset_scope
  return NULL;
}

void codegen(T_prog prog) {
  // no need to record symbols in the global offset_scope.  the assembler and linker handle them.
  current_offset_scope = NULL;
  label = 0;
  
  // emit assembly header
  printf(".file \"stdin\"\n");
  
  // emit a .comm for global vars
  global_symbols = create_table();
  // loop over each global symbol.  emit .comm and add to global symtab
  T_decllist decllist = prog->decllist;
  while (NULL != decllist) {
    if (E_functiontype != decllist->decl->type->kind) {
      printf(".comm\t%s,%d\n", decllist->decl->ident, bitwidth(decllist->decl->type));
      insert(global_symbols, decllist->decl->ident, decllist->decl->ident);
    }
    decllist = decllist->tail;
  }

  // emit an .rodata section and label for strings
  fprintf(stderr, "TODO: collect string constants\n");

  // go through each function
  codegen_funclist(prog->funclist);

  // generate the code for main
  codegen_main(prog->main);

  // free the global symbol table
  free(global_symbols);
}

static void codegen_main(T_main main) {
  // create a new scope
  current_offset_scope = create_offset_scope(NULL);

  // emit the pseudo ops for the function definition
  printf(".text\n");
  printf(".globl %s\n", "main");
  printf(".type %s, @function\n", "main");

  // emit a label for the function
  printf("%s:\n", "main");

  // add local declarations to the scope
  codegen_decllist(main->decllist);

  COMMENT("stack space for argc and argv");
  insert_offset(current_offset_scope, "argc", 8);  // int argc
  insert_offset(current_offset_scope, "argv", 8);  // char **argv

  COMMENT("emit main's prologue");
  emit_prologue(current_offset_scope->stack_size);

  COMMENT("move argc and argv from parameter registers to the stack");
  int offset;
  offset = lookup_offset_in_scope(current_offset_scope, "argc");
  MOV_TO_OFFSET("%rdi", offset);
  offset = lookup_offset_in_scope(current_offset_scope, "argv");
  MOV_TO_OFFSET("%rsi", offset);
  
  COMMENT("generate code for the body");
  codegen_stmtlist(main->stmtlist);

  COMMENT("generate code for the return expression");
  codegen_expr(main->returnexpr);
  COMMENT("save the return expression into %rax per the abi");
  POP("%rax");

  COMMENT("emit main's epilogue");
  emit_epilogue();

  // exit the scope
  current_offset_scope = destroy_offset_scope(current_offset_scope);
}

static void codegen_funclist(T_funclist funclist) {
  while (NULL != funclist) {
    codegen_func(funclist->func);
    funclist = funclist->tail;
  }
}

static void codegen_func(T_func func) {
  // Create new scope
  current_offset_scope = create_offset_scope(current_offset_scope);
  // Emit pseudo ops for function definition
  printf(".text\n");
  printf(".globl %s\n", func->ident);
  printf(".type %s, @function\n", func->ident);
  // Emit label
  printf("%s:\n", func->ident);

  // Place parameter in offset table
  insert_offset(current_offset_scope, func->paramlist->ident, 8);
  // Add local declarations to scope
  codegen_decllist(func->decllist);

  COMMENT("emit the function prologue");
  emit_prologue(current_offset_scope->stack_size);

  COMMENT("move parameter onto the stack");
  int offset = lookup_offset_in_scope(current_offset_scope, func->paramlist->ident);
  MOV_TO_OFFSET("%rdi", offset);

  COMMENT("generate code for the body");
  codegen_stmtlist(func->stmtlist);

  COMMENT("generate code for the return expression");
  codegen_expr(func->returnexpr);
  COMMENT("save the return expression into %rax per the abi");
  POP("%rax");

  COMMENT("emit the epilogue");
  emit_epilogue();

  // Exit the scope
  current_offset_scope = destroy_offset_scope(current_offset_scope);
}

// Insert all declarations into offset scope
static void codegen_decllist(T_decllist decllist) {
  while (NULL != decllist) {
      insert_offset(current_offset_scope, decllist->decl->ident, 8);
      decllist = decllist->tail;
  }
}

/* statements */
static void codegen_stmtlist(T_stmtlist stmtlist) {
  while (NULL != stmtlist) {
    codegen_stmt(stmtlist->stmt);
    stmtlist = stmtlist->tail;
  }
}

static void codegen_stmt(T_stmt stmt) {
  if (NULL == stmt) {
    fprintf(stderr, "FATAL: stmt is NULL in codegen_stmt\n");
    exit(1);
  }
  switch (stmt->kind) {
  case E_assignstmt: codegen_assignstmt(stmt); break;
  case E_ifstmt: codegen_ifstmt(stmt); break;
  case E_ifelsestmt: codegen_ifelsestmt(stmt); break;
  case E_whilestmt: codegen_whilestmt(stmt); break;
  case E_compoundstmt: codegen_compoundstmt(stmt); break;
  default: fprintf(stderr, "FATAL: unexpected stmt kind in codegen_stmt\n"); exit(1); break;
  }
}

static void codegen_assignstmt(T_stmt stmt) {
  if (E_identexpr == stmt->assignstmt.left->kind) {
    COMMENT("generate code for the right-hand side of the assignment");
    codegen_expr(stmt->assignstmt.right);
    POP("%rax");
    // Find offset of left-hand side and place right-hand side value into left-hand side address
    int offset = lookup_offset_in_scope(current_offset_scope, stmt->assignstmt.left->identexpr);
    MOV_TO_OFFSET("%rax", offset);
  }
  else if (E_unaryexpr == stmt->assignstmt.left->kind) {
    COMMENT("generate code for the deref on the left side of an expression");
    codegen_expr(stmt->assignstmt.left->unaryexpr.expr);
    COMMENT("generate code for the right-hand side of the assignment");
    codegen_expr(stmt->assignstmt.right);

    COMMENT("pop the result of the right-hand side");
    POP("%rax");
    COMMENT("pop the result of the left-hand side");
    POP("%rbx");
    COMMENT("move the right-hand side's value into the address pointed to by the left-hand size");
    MOV("%rax", "(%rbx)");
  }
}

static void codegen_ifstmt(T_stmt stmt) {
  COMMENT("generating code for an ifstmt");

  COMMENT("generate code for the expression");
  codegen_expr(stmt->ifstmt.cond);
  COMMENT("emit a pop of the expression value from the stack into a register");
  POP("%rax");

  COMMENT("emit a cmp of register's value to 0, i.e., check whether it's false");
  printf("\tcmp $0, %%rax\n");
  COMMENT("emit a je to the end label");
  printf("\tje .L%d\n", label);
  int endLabel = label++;

  COMMENT("generate code for the if branch");
  codegen_stmt(stmt->ifstmt.body);

  COMMENT("emit the end label");
  printf(".L%d:\n", endLabel);
}

static void codegen_ifelsestmt(T_stmt stmt) {
  COMMENT("generating code for an ifelsestmt");

  COMMENT("generate code for the expression");
  codegen_expr(stmt->ifelsestmt.cond);
  COMMENT("emit a pop of the expression value from the stack into a register");
  POP("%rax");

  COMMENT("emit a cmp of register's value to 0, i.e., check whether it's false");
  printf("\tcmp $0, %%rax\n");
  COMMENT("emit a je to the else label");
  printf("\tje .L%d\n", label);
  int elseLabel = label++;

  COMMENT("generate code for the if branch");
  codegen_stmt(stmt->ifelsestmt.ifbranch);
  COMMENT("emit a jump to the end label");
  printf("\tjmp .L%d\n", label);
  int endLabel = label++;

  COMMENT("emit the else label");
  printf(".L%d:\n", elseLabel);
  COMMENT("generate code for the else branch");
  codegen_stmt(stmt->ifelsestmt.elsebranch);

  COMMENT("emit the end label");
  printf(".L%d:\n", endLabel);
}

static void codegen_whilestmt(T_stmt stmt) {
  COMMENT("generating code for a whilestmt");

  COMMENT("emit the head label");
  printf(".L%d:\n", label);
  int headLabel = label++;

  COMMENT("generate code for the expression");
  codegen_expr(stmt->whilestmt.cond);
  COMMENT("emit a pop of the expression value from the stack into a register");
  POP("%rax");

  COMMENT("emit a cmp of register's value to 0, i.e., check whether it's false");
  printf("\tcmp $0, %%rax\n");
  COMMENT("emit a je to the end label");
  printf("\tje .L%d\n", label);
  int endLabel = label++;

  COMMENT("generate code for the while body");
  codegen_stmt(stmt->whilestmt.body);
  COMMENT("emit a jump to the head label");
  printf("\tjmp .L%d\n", headLabel);

  COMMENT("emit the end label");
  printf(".L%d:\n", endLabel);
}

static void codegen_compoundstmt(T_stmt stmt) {
  codegen_stmtlist(stmt->compoundstmt.stmtlist);
}

/* expressions */
static void codegen_expr(T_expr expr) {
  if (NULL == expr) {
    fprintf(stderr, "FATAL: unexpected NULL in codegen_expr\n");
    exit(1);
  }
  switch (expr->kind) {
  case E_identexpr: codegen_identexpr(expr); break;
  case E_callexpr: codegen_callexpr(expr); break;
  case E_intexpr: codegen_intexpr(expr); break;
  case E_charexpr: codegen_charexpr(expr); break;
  case E_strexpr: codegen_strexpr(expr); break;
  case E_arrayexpr: codegen_arrayexpr(expr); break;
  case E_unaryexpr: codegen_unaryexpr(expr); break;
  case E_binaryexpr: codegen_binaryexpr(expr); break;
  case E_castexpr: codegen_castexpr(expr); break;
  default: fprintf(stderr, "FATAL: unexpected expr kind in codegen_expr\n"); exit(1); break;
  }
}

static void codegen_identexpr(T_expr expr) {
  // look up the ident, then move it to an intermediate register
  int offset = lookup_offset_in_scope(current_offset_scope, expr->identexpr);
  MOV_FROM_OFFSET(offset, "%rax");
  PUSH("%rax");
}

static void codegen_callexpr(T_expr expr) {
  COMMENT("evaluate the parameter");
  codegen_expr(expr->callexpr.args->expr);

  COMMENT("pass the parameter");
  POP("%rdi");

  COMMENT("call the function");
  CALL(expr->callexpr.ident);
  COMMENT("push the return value");
  PUSH("%rax");
}

static void codegen_intexpr(T_expr expr) {
  COMMENT("push the integer");
  MOV_FROM_IMMEDIATE(expr->intexpr, "%rax");
  PUSH("%rax");
}

static void codegen_charexpr(T_expr expr) {
  COMMENT("push the character");
  MOV_FROM_IMMEDIATE((int) expr->charexpr, "%rax");
  PUSH("%rax");
}

static void codegen_strexpr(T_expr expr) {
  // bonus exercise
}

static void codegen_arrayexpr(T_expr expr) {
  // bonus exercise
}

static void codegen_unaryexpr(T_expr expr) {
  // Reference operator
  if (E_op_ref == expr->unaryexpr.op) {
    // Only allow referencing an identifier
    if (E_identexpr != expr->unaryexpr.expr->kind) {
        fprintf(stderr, "FATAL: unexpected expr kind in codegen_unaryexpr\n");
        exit(1);
    }

    COMMENT("copy the base pointer to another register");
    MOV("%rbp", "%rax");

    int offset = lookup_offset_in_scope(current_offset_scope, expr->unaryexpr.expr->identexpr);
    COMMENT("subtract the offset from the copy of the base pointer");
    SUBCONST(offset, "%rax");

    COMMENT("push the expression result");
    PUSH("%rax");
  }
  // Dereference operator
  else if (E_op_deref == expr->unaryexpr.op) {
    COMMENT("generate code for the unary operand");
    codegen_expr(expr->unaryexpr.expr);
    COMMENT("pop the unary operand");
    POP("%rax");

    COMMENT("move the contents of the address to the result register");
    MOV("(%rax)", "%rbx");
    COMMENT("push the expression result");
    PUSH("%rbx");
  }
  else if (E_op_not == expr->unaryexpr.op) {
    codegen_expr(expr->unaryexpr.expr);
    POP("%rax");

    printf("\tcmp $0, %%rax\n");
    printf("\tje .L%d\n", label);
    MOV_FROM_IMMEDIATE(0, "%rax");
    printf("\tjmp .L%d\n", ++label);

    printf(".L%d:\n", label - 1);
    MOV_FROM_IMMEDIATE(1, "%rax");

    printf(".L%d:\n", label++);
    PUSH("%rax");
  }
  else {
    fprintf(stderr, "FATAL: unexpected op kind in codegen_unaryexpr\n"); 
    exit(1);
  }
}

static void codegen_binaryexpr(T_expr expr) {
  COMMENT("generate code for the left operand");
  codegen_expr(expr->binaryexpr.left);
  COMMENT("generate code for the right operand");
  codegen_expr(expr->binaryexpr.right);

  COMMENT("pop the right operand");
  POP("%rbx");
  COMMENT("pop the left operand");
  POP("%rax");

  switch (expr->binaryexpr.op) {
  case E_op_plus:
    COMMENT("do the addition");
    ADD("%rbx", "%rax");
    break;
  case E_op_minus:
    COMMENT("do the subtraction");
    SUB("%rbx", "%rax");
    break;
  case E_op_times:
    COMMENT("do the multiplication");
    IMUL("%rbx", "%rax");
    break;
  case E_op_divide:
    COMMENT("do the division");
    CDQ();
    IDIV("%rbx");
    break;
  case E_op_mod:
    COMMENT("do the remainder");
    CDQ();
    IDIV("%rbx");
    MOV("%rdx", "%rax");
    break;
  case E_op_and:
    printf("\tcmp $0, %%rax\n");
    printf("\tje .L%d\n", label);
    printf("\tcmp $0, %%rbx\n");
    printf("\tje .L%d\n", label);

    MOV_FROM_IMMEDIATE(1, "%rax");
    printf("\tjmp .L%d\n", ++label);

    printf(".L%d:\n", label - 1);
    MOV_FROM_IMMEDIATE(0, "%rax");
    printf(".L%d:\n", label++);
    break;
  case E_op_or:
    printf("\tcmp $0, %%rax\n");
    printf("\tjne .L%d\n", label);
    printf("\tcmp $0, %%rbx\n");
    printf("\tjne .L%d\n", label);

    MOV_FROM_IMMEDIATE(0, "%rax");
    printf("\tjmp .L%d\n", ++label);

    printf(".L%d:\n", label - 1);
    MOV_FROM_IMMEDIATE(1, "%rax");
    printf(".L%d:\n", label++);
    break;
  case E_op_eq:
    printf("\tcmp %%rax, %%rbx\n");
    printf("\tje .L%d\n", label);
    MOV_FROM_IMMEDIATE(0, "%rax");
    printf("\tjmp .L%d\n", ++label);

    printf(".L%d:\n", label - 1);
    MOV_FROM_IMMEDIATE(1, "%rax");
    printf(".L%d:\n", label++);
    break;
  case E_op_lt:
    printf("\tcmp %%rax, %%rbx\n");
    printf("\tjge .L%d\n", label);
    MOV_FROM_IMMEDIATE(0, "%rax");
    printf("\tjmp .L%d\n", ++label);

    printf(".L%d:\n", label - 1);
    MOV_FROM_IMMEDIATE(1, "%rax");
    printf(".L%d:\n", label++);
    break;
  default: fprintf(stderr, "FATAL: unexpected op kind in codegen_binaryexpr\n"); exit(1); break;
  }

  COMMENT("push the expression result");
  PUSH("%rax");
}

static void codegen_castexpr(T_expr expr) {
  codegen_expr(expr->castexpr.expr);
}

/**
 * Emit a function prologue, given some size in bytes of the local
 * variables in the stack frame.
 */
static void emit_prologue(int size) {
	//save stack
	PUSH("%rbp");
	MOV("%rsp", "%rbp");
	if (size > 0) {
		SUBCONST(size, "%rsp");
	}
	PUSH("%rbx");
}

static void emit_epilogue() {
	POP("%rbx");
	MOV("%rbp", "%rsp");
	POP("%rbp");
	RET;
}

/**
 * This function returns the size of a type in bytes.
 */
static int bitwidth(T_type type) {
  switch (type->kind) {
  case E_primitivetype:
    switch (type->primitivetype) {
    case E_typename_int:
      // 32-bit integers
      return 4;
    case E_typename_char:
      // characters are 1 byte
      return 1;
    default:
      fprintf(stderr, "FATAL: unexpected kind in compare_types\n");
      exit(1);
    }
    break;
  case E_pointertype:
    // pointers are to 64-bit addresses
    return 8;
  case E_arraytype:
    // arrays are the size times the bitwidth of the type
    return type->arraytype.size * bitwidth(type->arraytype.type);
  case E_functiontype:
    fprintf(stderr, "FATAL: functions as values are not supported\n");
    exit(1);
    break;
  default:
    fprintf(stderr, "FATAL: unexpected kind in bitwidth\n");
    exit(1);
    break;
  }  
}
