(* File memory.ml *)
module StringMap = Map.Make(String)

type var = int

type env = var StringMap.t

(* Basic functions *)
let empty_env : env = StringMap.empty

let lookup_var loc env = 
  try StringMap.find loc env
  with Not_found -> failwith ("Variable " ^ loc ^ " not found")

let update_var loc value env = StringMap.add loc value env

let string_of_env env = 
  let vars_str = 
    StringMap.bindings env
    |> List.map (fun (k, v) -> Printf.sprintf "%s -> %d" k v)
    |> String.concat ", "
  in
  Printf.sprintf "Variables: { %s }" vars_str

let rec affect_var env var =
  Printf.printf "Enter variable %s\n" var;
  try 
    update_var var (read_int ()) env
  with
  |_ ->
      print_endline "Error: Invalid input. Please enter an integer.";
      affect_var env var

let init_env vars = List.fold_left (fun acc var -> affect_var acc var) empty_env vars
  