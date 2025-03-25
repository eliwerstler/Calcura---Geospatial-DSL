(* interpreter.ml *)
open Ast

type value =
  | IntVal of int
  | StringVal of string

let rec eval_expr = function
  | Literal (IntLit i) ->
      IntVal i
  | Literal (StringLit s) ->
      StringVal s
  | Binop (e1, Add, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      (match (v1, v2) with
      | (IntVal i1, IntVal i2) ->
          IntVal (i1 + i2)
      | _ -> failwith "Type error: Add operation requires two integers")
  | Binop (e1, Sub, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      (match (v1, v2) with
      | (IntVal i1, IntVal i2) ->
          IntVal (i1 - i2)
      | _ -> failwith "Type error: Sub operation requires two integers")
  | Binop (e1, Mult, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      (match (v1, v2) with
      | (IntVal i1, IntVal i2) ->
          IntVal (i1 * i2)
      | _ -> failwith "Type error: Mult operation requires two integers")
  | Binop (e1, Div, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      (match (v1, v2) with
      | (IntVal i1, IntVal i2) ->
          if i2 = 0 then failwith "Runtime error: Division by zero"
          else IntVal (i1 / i2)
      | _ -> failwith "Type error: Div operation requires two integers")
  | Binop (e1, Mod, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      (match (v1, v2) with
      | (IntVal i1, IntVal i2) ->
          if i2 = 0 then failwith "Runtime error: Modulo by zero"
          else IntVal (i1 mod i2)
      | _ -> failwith "Type error: Mod operation requires two integers")
  | _ ->
      failwith "unimplemented"

let eval_stmt = function
  | Expr e ->
      ignore (eval_expr e)
  | Print e ->
      (match eval_expr e with
      | IntVal i -> 
          print_endline (string_of_int i)
      | StringVal s -> 
          print_endline s
      )
  | _ ->
      failwith "unimplemented"

let run (prog: program) =
  List.iter eval_stmt prog
