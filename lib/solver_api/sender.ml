open Smtlib_converter

let parse_z3_output input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.main Lexer.token lexbuf
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let line = pos.Lexing.pos_lnum in
      let char = lexbuf.Lexing.lex_curr_pos - pos.Lexing.pos_bol in
      Printf.eprintf "Parse error at line %d, character %d\n" line char;
      Unknown
  | Lexer.Error msg ->
      Printf.eprintf "Lexer error: %s\n" msg;
      Unknown
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      Unknown


let send_to_z3 formula = 
  let vars = Utils.get_vars_from_formula formula in
  let message = 
    Lemmas.prefix ^
    (List.fold_left (fun acc loc -> Printf.sprintf "(declare-const %s Int)\n%s" loc acc) "" (Utils.remove_duplicates vars)) ^
    Printf.sprintf "(assert %s)\n" (Smtlib_converter.smtlib_of_formula formula) ^
    "(check-sat)\n(get-model)"
  in
  
  let oc = open_out "output/smtlib.smt2" in
  Printf.fprintf oc "%s\n" message;
  close_out oc;

  let output = Utils.run_terminal "z3 output/smtlib.smt2" in
  parse_z3_output (output)
