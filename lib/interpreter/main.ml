open Eval
open Memory

let read_file file_name =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin file_name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let runProg command vars = 
  let start_env = init_env vars in
  print_endline "---------------EXECUTION------------------";
  let env = execute command start_env in
  print_endline "\n------------------END---------------------";
  print_endline "Program terminated successfully !";
  print_endline "Final environment:";
  print_string (string_of_env env)

