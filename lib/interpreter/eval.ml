(* File eval.ml *)
open Core.Ast
open Memory
open Stdlib

let rec evalAexp (arith : arithmetic) (env : env) : int = match arith with
  |Int n -> n
  |Var s -> lookup_var s env
  |Neg a -> - evalAexp a env
  
  |Add (a1, a2) -> (evalAexp a1 env) + (evalAexp a2 env)
  |Mul (a1, a2) -> (evalAexp a1 env) * (evalAexp a2 env)
  |Div (a1, a2) -> 
    let divisor = evalAexp a2 env in
    if divisor = 0 then failwith "Division by zero"
    else (evalAexp a1 env) / divisor
  |Mod (a1, a2) ->
    let divisor = evalAexp a2 env in
    if divisor = 0 then failwith "Division by zero"
    else (evalAexp a1 env) mod divisor

let rec evalBexp (bexp : boolean) (env : env) : bool = match bexp with
  |True -> true
  |False -> false
  |Not b -> not (evalBexp b env)
  |And (b1, b2) ->
    let r1 = evalBexp b1 env in
    if not r1 then false
    else evalBexp b2 env
  |Or (b1, b2) ->
    let r1 = evalBexp b1 env in
    if r1 then true
    else evalBexp b2 env
  |Eq (a1, a2) -> (evalAexp a1 env) = (evalAexp a2 env)
  |Leq (a1, a2) -> (evalAexp a1 env) <= (evalAexp a2 env)
  |Lt (a1, a2) -> (evalAexp a1 env) < (evalAexp a2 env)
  |Gt (a1, a2) -> (evalAexp a1 env) > (evalAexp a2 env)
  |Geq (a1, a2) -> (evalAexp a1 env) >= (evalAexp a2 env)

let rec execute (c : command) (env : env) = match c with
  |Skip -> env (* No changes *)
  |Assign (s, a) -> 
    let result = evalAexp a env in
    update_var s (result) env
  |If (b, c1, c2) -> 
    if evalBexp b env then execute c1 env
    else execute c2 env
  |While (b, _, c) -> 
    let rec loop env = 
      if evalBexp b env then 
        loop (execute c env)
      else env
    in loop env
  |Seq (c1, c2) -> 
    let env_after_c1 = execute c1 env in
    execute c2 env_after_c1