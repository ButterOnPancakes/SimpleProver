open Core.Ast
open Printf

type smt_result =
  | Unsat of string option
  | Sat of (string * int) list
  | Unknown

let rec smtlib_of_arith a = match a with
  | Int i -> string_of_int i
  | Var v -> v
  | Neg a -> sprintf "(- %s)" (smtlib_of_arith a)
  | Add (a1, a2) -> sprintf "(+ %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)
  | Mul (a1, a2) -> sprintf "(* %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)
  | Div (a1, a2) -> sprintf "(div %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)
  | Mod (a1, a2) -> sprintf "(mod %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)

let rec smtlib_of_formula f = match f with
  | TrueF -> "true"
  | FalseF -> "false"
  | NotF f -> sprintf "(not %s)" (smtlib_of_formula f)
  | AndF (f1, f2) -> sprintf "(and %s %s)" (smtlib_of_formula f1) (smtlib_of_formula f2)
  | OrF (f1, f2) -> sprintf "(or %s %s)" (smtlib_of_formula f1) (smtlib_of_formula f2)
  | ImplyF (f1, f2) -> sprintf "(=> %s %s)" (smtlib_of_formula f1) (smtlib_of_formula f2)
  | ForallF (x, f) -> sprintf "(forall ((%s Int)) %s)" x (smtlib_of_formula f)
  | ExistsF (x, f) -> sprintf "(exists ((%s Int)) %s)" x (smtlib_of_formula f)
  | EqF (a1, a2) -> sprintf "(= %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)
  | LeqF (a1, a2) -> sprintf "(<= %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)
  | LtF (a1, a2) -> sprintf "(< %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)
  | GtF (a1, a2) -> sprintf "(> %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)
  | GeqF (a1, a2) -> sprintf "(>= %s %s)" (smtlib_of_arith a1) (smtlib_of_arith a2)

