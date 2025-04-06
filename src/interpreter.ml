(* interpreter.ml *)
open Ast

type value =
  | IntVal of int
  | StringVal of string

module Env = Map.Make(String)

let env : value Env.t ref = ref Env.empty

let rec eval_expr = function
  | Id name ->
      (try Env.find name !env
      with Not_found -> failwith ("Unbound variable: " ^ name))
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
  | Binop (e1, Greater, e2) ->
      (match (eval_expr e1, eval_expr e2) with
      | IntVal i1, IntVal i2 -> IntVal (if i1 > i2 then 1 else 0)
      | _ -> failwith "Type error in '>' comparison")
  | Binop (e1, Less, e2) ->
      (match (eval_expr e1, eval_expr e2) with
      | IntVal i1, IntVal i2 -> IntVal (if i1 < i2 then 1 else 0)
      | _ -> failwith "Type error in '<' comparison")
  | Binop (e1, Equal, e2) ->
      (match (eval_expr e1, eval_expr e2) with
      | IntVal i1, IntVal i2 -> IntVal (if i1 = i2 then 1 else 0)
      | StringVal s1, StringVal s2 -> IntVal (if s1 = s2 then 1 else 0)
      | _ -> failwith "Type mismatch in equality check")
  | Binop (e1, Neq, e2) ->
      (match (eval_expr e1, eval_expr e2) with
      | IntVal i1, IntVal i2 -> IntVal (if i1 <> i2 then 1 else 0)
      | StringVal s1, StringVal s2 -> IntVal (if s1 <> s2 then 1 else 0)
      | _ -> failwith "Type mismatch in inequality check")
  | _ ->
      failwith "unimplemented"

let rec eval_stmt = function
  | Decl (name, _, expr) ->
      let value = eval_expr expr in
      env := Env.add name value !env
  | Expr e ->
      ignore (eval_expr e)
  | Print e ->
      (match eval_expr e with
      | IntVal i -> 
          print_endline (string_of_int i)
      | StringVal s -> 
          print_endline s
      )
  | If (cond_expr, then_branch, else_opt) ->
      (match eval_expr cond_expr with
      | IntVal v when v <> 0 -> List.iter eval_stmt then_branch
      | IntVal 0 ->
        (match else_opt with
         | Some else_branch -> List.iter eval_stmt else_branch
         | None -> ())
      | _ -> failwith "Condition must evaluate to an integer") 
  | _ ->
      failwith "unimplemented"

let run (prog: program) =
  List.iter eval_stmt prog
