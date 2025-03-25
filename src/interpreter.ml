(* interpreter.ml *)
open Ast

type value =
  | IntVal of int
  | StringVal of string

let rec eval_expr = function
  | Literal (IntLit i) ->
      Printf.printf "Evaluating IntLit: %d\n" i;
      IntVal i
  | Literal (StringLit s) ->
      Printf.printf "Evaluating StringLit: %s\n" s;
      StringVal s
  | Binop (e1, Add, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      (match (v1, v2) with
      | (IntVal i1, IntVal i2) ->
          Printf.printf "Evaluating Add: %d + %d\n" i1 i2;
          IntVal (i1 + i2)
      | _ -> failwith "Type error: Add operation requires two integers")
  | Binop (e1, Sub, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      (match (v1, v2) with
      | (IntVal i1, IntVal i2) ->
          Printf.printf "Evaluating Sub: %d - %d\n" i1 i2;
          IntVal (i1 - i2)
      | _ -> failwith "Type error: Sub operation requires two integers")
  | _ ->
      Printf.printf "Unimplemented expression evaluation\n";
      failwith "unimplemented"

let eval_stmt = function
  | Expr e ->
      Printf.printf "Evaluating expression statement\n";
      ignore (eval_expr e)
  | Print e ->
      Printf.printf "Evaluating print statement\n";
      (match eval_expr e with
      | IntVal i -> Printf.printf "%d\n" i
      | StringVal s -> Printf.printf "%s\n" s)
  | _ ->
      Printf.printf "Unimplemented statement evaluation\n";
      failwith "unimplemented"

let run (prog: program) =
  Printf.printf "Running program\n";
  List.iter eval_stmt prog
