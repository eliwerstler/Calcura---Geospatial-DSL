(* interpreter.ml *)
open Ast

type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | StringVal of string
  | IntList of int list
  | FloatList of float list


module Env = Map.Make(String)

let env : value Env.t ref = ref Env.empty

let rec eval_expr (e : expr) : value = 
    match e with
    | Id name ->
        (try Env.find name !env
         with Not_found -> failwith ("Unbound variable: " ^ name))
  
    | Literal (IntLit i) -> IntVal i
    | Literal (FloatLit f) -> FloatVal f
    | Literal (StringLit s) -> StringVal s
    | Literal (BoolLit b) -> BoolVal b
    | Literal (IntListLit iL) -> IntList iL
    | Literal (FloatListLit fL) -> FloatList fL
(*   
    | Index (list_expr, index_expr) ->
        let list_val = eval_expr list_expr in
        let index_val = eval_expr index_expr in
        match (list_val, index_val) with
        | IntList xs, IntVal i ->
            if i < 0 || i >= List.length xs then failwith "Index out of bounds"
            else IntVal (List.nth xs i)
        | FloatList xs, IntVal i ->
            if i < 0 || i >= List.length xs then failwith "Index out of bounds"
            else FloatVal (List.nth xs i)
        | _ -> failwith "Invalid list indexing" *)
  
    | Binop (e1, Add, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal i1, IntVal i2 -> IntVal (i1 + i2)
         | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
         | IntVal i, FloatVal f | FloatVal f, IntVal i -> FloatVal (float_of_int i +. f)
         | _ -> failwith "Type error: Add requires numbers")
  
    | Binop (e1, Sub, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal i1, IntVal i2 -> IntVal (i1 - i2)
         | FloatVal f1, FloatVal f2 -> FloatVal (f1 -. f2)
         | IntVal i, FloatVal f -> FloatVal (float_of_int i -. f)
         | FloatVal f, IntVal i -> FloatVal (f -. float_of_int i)
         | _ -> failwith "Type error: Sub requires numbers")
  
    | Binop (e1, Mult, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal i1, IntVal i2 -> IntVal (i1 * i2)
         | FloatVal f1, FloatVal f2 -> FloatVal (f1 *. f2)
         | IntVal i, FloatVal f | FloatVal f, IntVal i -> FloatVal (float_of_int i *. f)
         | _ -> failwith "Type error: Mult requires numbers")
  
    | Binop (e1, Div, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal _, IntVal 0 | FloatVal _, FloatVal 0.0 -> failwith "Division by zero"
         | IntVal i1, IntVal i2 -> IntVal (i1 / i2)
         | FloatVal f1, FloatVal f2 -> FloatVal (f1 /. f2)
         | IntVal i, FloatVal f -> FloatVal (float_of_int i /. f)
         | FloatVal f, IntVal i -> FloatVal (f /. float_of_int i)
         | _ -> failwith "Type error: Div requires numbers")
  
    | Binop (e1, Mod, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal _, IntVal 0 -> failwith "Modulo by zero"
         | IntVal i1, IntVal i2 -> IntVal (i1 mod i2)
         | _ -> failwith "Mod only works with integers")
  
    | Binop (e1, Greater, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal i1, IntVal i2 -> IntVal (if i1 > i2 then 1 else 0)
         | FloatVal f1, FloatVal f2 -> IntVal (if f1 > f2 then 1 else 0)
         | IntVal i, FloatVal f -> IntVal (if float_of_int i > f then 1 else 0)
         | FloatVal f, IntVal i -> IntVal (if f > float_of_int i then 1 else 0)
         | _ -> failwith "Type error in '>' comparison")
  
    | Binop (e1, Less, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal i1, IntVal i2 -> IntVal (if i1 < i2 then 1 else 0)
         | FloatVal f1, FloatVal f2 -> IntVal (if f1 < f2 then 1 else 0)
         | IntVal i, FloatVal f -> IntVal (if float_of_int i < f then 1 else 0)
         | FloatVal f, IntVal i -> IntVal (if f < float_of_int i then 1 else 0)
         | _ -> failwith "Type error in '<' comparison")
  
    | Binop (e1, Equal, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal i1, IntVal i2 -> IntVal (if i1 = i2 then 1 else 0)
         | FloatVal f1, FloatVal f2 -> IntVal (if f1 = f2 then 1 else 0)
         | IntVal i, FloatVal f | FloatVal f, IntVal i -> IntVal (if float_of_int i = f then 1 else 0)
         | StringVal s1, StringVal s2 -> IntVal (if s1 = s2 then 1 else 0)
         | _ -> failwith "Type mismatch in equality check")
  
    | Binop (e1, Neq, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
         | IntVal i1, IntVal i2 -> IntVal (if i1 <> i2 then 1 else 0)
         | FloatVal f1, FloatVal f2 -> IntVal (if f1 <> f2 then 1 else 0)
         | IntVal i, FloatVal f | FloatVal f, IntVal i -> IntVal (if float_of_int i <> f then 1 else 0)
         | StringVal s1, StringVal s2 -> IntVal (if s1 <> s2 then 1 else 0)
         | _ -> failwith "Type mismatch in inequality check")
  
    | _ -> failwith "unimplemented"
  
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
      | FloatVal f -> 
          print_endline (string_of_float f)
      | StringVal s -> 
          print_endline s
      | BoolVal b -> print_endline (string_of_bool b)
      | IntList iL -> 
          let s = "[" ^ String.concat ", " (List.map string_of_int iL) ^ "]" in
          print_endline s
      | FloatList fL ->
          let s = "[" ^ String.concat ", " (List.map string_of_float fL) ^ "]" in
          print_endline s
      )
  | If (cond_expr, then_branch, else_opt) ->
      (match eval_expr cond_expr with
      | IntVal v when v <> 0 -> List.iter eval_stmt then_branch
      | IntVal 0 -> (match else_opt with Some else_branch -> List.iter eval_stmt else_branch | None -> ())
      | BoolVal true -> List.iter eval_stmt then_branch
      | BoolVal false -> (match else_opt with Some else_branch -> List.iter eval_stmt else_branch | None -> ())
      | _ -> failwith "Condition must evaluate to an integer or boolean") 
  | _ ->
      failwith "unimplemented"

let run (prog: program) =
  List.iter eval_stmt prog
