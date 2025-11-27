%{
  open Smtlib_converter
%}

%token <int> INT
%token <string> ID
%token SAT UNSAT UNKNOWN DEFINE_FUN INT_TYPE UNIT MINUS
%token LPAREN RPAREN EOF

%start <smt_result> main

%%

main:
  | SAT model EOF     { Sat $2 }
  | UNSAT EOF         { Unsat None }
  | UNKNOWN EOF       { Unknown }
  ;

model:
  | LPAREN definitions RPAREN   { $2 }
  | /* empty */                 { [] }
  ;

definitions:
  | /* empty */                 { [] }
  | definition definitions      { $1 :: $2 }
  ;

definition:
  | LPAREN DEFINE_FUN ID UNIT INT_TYPE expression RPAREN
    { ($3, $6) }
  ;

expression:
  | INT                         { $1 }
  | LPAREN MINUS INT RPAREN       { -$3 }
  ;

%%