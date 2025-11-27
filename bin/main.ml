open Core.Utils
open Core.Ast
open Parser.Main
open Solver_api.Smtlib_converter

let print_output = function
    |Unsat _ -> print_endline "Program valid"
    |Sat model ->
      let model_str = List.fold_left (fun acc (var, value) -> Printf.sprintf "%s%s = %d\n" acc var value) "" model in
      print_endline (Printf.sprintf "Program not valid\nCounter-example:\n%s\nStopping verification." model_str)
    |Unknown -> print_endline "Solver returned Unknown"

let rec verif_vcs = function
  |[] -> print_endline "End of verification conditions (All Valid)."; true
  |h::t -> 
    print_endline (string_of_formula h);
    let output = Solver_api.Sender.send_to_z3 (NotF h) in match output with
    |Unsat _ -> verif_vcs t
    |Sat model ->
      let model_str = List.fold_left (fun acc (var, value) -> Printf.sprintf "%s%s = %d\n" acc var value) "" model in
      print_endline (Printf.sprintf "VC not valid\nCounter-example:\n%s\nStopping verification." model_str);
      false
    |Unknown -> print_endline "VC verification returned Unknown\nStopping verification.";
      false
    
let () = 
  if Array.length Sys.argv <> 2 then failwith "Usage: ./main <filename>";
  let filename = Sys.argv.(1) in
  let pg = parse (read_file filename) in

  let (wp, vcs) = Prover.Vc_gen.wp pg.body pg.postcondition in

  print_endline (string_of_program pg);

  print_endline "\n---------------CORRECTION---------------\n";
  print_endline "VCs : ";
  if (not (verif_vcs vcs)) then exit 1;
  print_newline ();

  print_endline "Weakest precondition : ";
  print_endline (Core.Utils.string_of_formula wp);
  print_endline "\nResult :";
  print_output (Solver_api.Sender.send_to_z3 (NotF (ImplyF (pg.precondition, wp))));

  print_string "\n-------------------END-------------------\n";