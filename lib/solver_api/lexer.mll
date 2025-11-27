{
open Parser
exception Error of string
}

let white = [' ' '\t' '\r']+
let newline = '\n'
let digit = ['0'-'9']
let int = '-'? digit+
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*

rule token = parse
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | '-' { MINUS }
  | "sat"       { SAT }
  | "unsat"     { UNSAT }
  | "unknown"   { UNKNOWN }
  | "define-fun" { DEFINE_FUN }
  | "Int"       { INT_TYPE }
  | "()"        { UNIT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | int         { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ident       { ID (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _ { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }