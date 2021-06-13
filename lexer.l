D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]

%{
#include <stdio.h>

#include "util.h"
#include "ast.h"

/* declare tokens defined by parser.y. */
#include "parser.tab.h"

/* update position in input file. */
void count();

void comment();
%}

%option yylineno

%%
"/*"       { comment(); }

"char"     { count(); return(CHAR); }
"else"     { count(); return(ELSE); }
"if"       { count(); return(IF); }
"int"      { count(); return(INT); }
"return"   { count(); return(RETURN); }
"while"    { count(); return(WHILE); }
"array"    { count(); return(ARRAY); }
"pointer"  { count(); return(POINTER); }
"function" { count(); return(FUNCTION); }
"main"     { count(); return(MAIN); }

{L}({L}|{D})*  { count(); yylval.strval = tostrval(yytext); return(IDENTIFIER); }
0[xX]{H}       { count(); yylval.intval = tointval(yytext, 16); return(HEX_CONSTANT); }
{D}+           { count(); yylval.intval = tointval(yytext, 10); return(INT_CONSTANT); }
('(([\\]['])|([^']))+')  { count(); yylval.charval = tocharval(yytext); return(CHAR_CONSTANT); } // sprintf to escape, truncate to one char

\"(\\.|[^\\"])*\"  { count(); yylval.strval = tostrval(yytext); return(STRING_LITERAL); }

"->"     { count(); return(ARROW); }
";"      { count(); return(';'); }
":"      { count(); return(':'); }
"{"      { count(); return('{'); }
"}"      { count(); return('}'); }
","      { count(); return(','); }
"="      { count(); return('='); }
"("      { count(); return('('); }
")"      { count(); return(')'); }
"["      { count(); return('['); }
"]"      { count(); return(']'); }
"&&"     { count(); return(AND_OP); }
"||"     { count(); return(OR_OP); }
"<="     { count(); return(LE_OP); }
">="     { count(); return(GE_OP); }
"=="     { count(); return(EQ_OP); }
"!="     { count(); return(NE_OP); }
"<"      { count(); return('<'); }
">"      { count(); return('>'); }
"&"      { count(); return('&'); }
"!"      { count(); return('!'); }
"-"      { count(); return('-'); }
"+"      { count(); return('+'); }
"*"      { count(); return('*'); }
"/"      { count(); return('/'); }
"%"      { count(); return('%'); }

[ \t\v\n\f]    { count(); }
.      { /* ignore bad characters */ }

%%

int yywrap()
{
  return(1);
}

void comment()
{
  char c, c1;

loop:
  while ((c = input()) != '*' && c != 0) {
    // putchar(c);
  }

  if ((c1 = input()) != '/' && c != 0)
  {
    // unput(c1);
    goto loop;
  }

  if (c != 0) {
    // putchar(c1);
  }
}

int column = 0;

void count()
{
  int i;

  for (i = 0; yytext[i] != '\0'; i++)
    if (yytext[i] == '\n')
      column = 0;
    else if (yytext[i] == '\t')
      column += 8 - (column % 8);
    else
      column++;

  ECHO;
}
