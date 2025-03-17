(* interpreter.ml *)
open Ast

(* TODO: Extend eval_expr to handle all expression types *)
let rec eval_expr = function
  | Literal(IntLit i) -> i
  | Binop(e1, Add, e2) -> eval_expr e1 + eval_expr e2
  | Binop(e1, Sub, e2) -> eval_expr e1 - eval_expr e2
  | _ -> failwith "unimplemented"

(* TODO: Extend eval_stmt to handle all statement types *)
let eval_stmt = function
  | Expr e -> ignore (eval_expr e)
  | Print e -> Printf.printf "%d\n" (eval_expr e)
  | _ -> failwith "unimplemented"

(* TODO: Add proper environment handling for variables and scopes *)
let run (prog: program) =
  List.iter eval_stmt prog
