open Core.Ast

let run_terminal cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp; r

let rec remove_duplicates l =
  let rec contains l n =
    match l with
    | [] -> false
    | h :: t ->
      h = n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
    let acc = remove_duplicates t in
    if contains acc h then acc else h :: acc

let rec get_vars_from_arith = function
  |Int _ -> []
  |Var v -> [v]
  |Neg a -> get_vars_from_arith a
  |Add (a, b) | Mul (a, b) | Div (a, b) | Mod (a, b) -> get_vars_from_arith a @ get_vars_from_arith b

let rec get_vars_from_formula = function
  |TrueF | FalseF -> []
  |NotF f -> get_vars_from_formula f
  |AndF (f1, f2) | OrF (f1, f2) | ImplyF (f1, f2) ->
    get_vars_from_formula f1 @ get_vars_from_formula f2
  |ForallF (x, f) | ExistsF (x, f) -> List.filter ((<>) x) (get_vars_from_formula f)
  |EqF (a1, a2) | LeqF (a1, a2) | LtF (a1, a2) | GtF (a1, a2) | GeqF (a1, a2) -> get_vars_from_arith a1 @ get_vars_from_arith a2