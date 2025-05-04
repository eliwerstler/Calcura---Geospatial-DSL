(* interpreter.ml *)
open Ast

type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | StringVal of string
  | IntList of int list
  | FloatList of float list
  | IntTuple of int list
  | FloatTuple of float list

module Env = Map.Make(String)

let env : value Env.t ref = ref Env.empty

let transform_list lst from_system to_system =
  match (from_system, to_system) with
  | "WGS84", "UTM" -> List.map (fun x -> x + 1) lst
  | "UTM", "WGS84" -> List.map (fun x -> x - 1) lst
  | _ -> failwith "Unsupported transformation"

let sort_list list_val ascending =
  match list_val with
  | IntList lst ->
      let sorted = List.sort compare lst in
      IntList (if ascending then sorted else List.rev sorted)
  | FloatList lst ->
      let sorted = List.sort compare lst in
      FloatList (if ascending then sorted else List.rev sorted)
  | _ -> failwith "Sort only works with lists of integers or floats"

let rec eval_expr (e : expr) : value =
    match e with
    | Id name ->
        (try Env.find name !env
         with Not_found -> failwith ("Unbound variable: " ^ name))

    | Literal (IntLit i) -> IntVal i
    | Literal (FloatLit f) -> FloatVal f
    | Literal (StringLit s) -> StringVal s
    | Literal (BoolLit b) -> BoolVal b
    | Literal (CharLit c) -> StringVal (String.make 1 c)
    | Literal (IntListLit iL) -> IntList iL
    | Literal (FloatListLit fL) -> FloatList fL
    | Literal (IntTupleLit iT) -> IntTuple iT
    | Literal (FloatTupleLit fT) -> FloatTuple fT
    | ListExpr exprs ->
        let values = List.map eval_expr exprs in
        (match values with
        | (IntVal _)::_ ->
            IntList (List.map (function IntVal i -> i | _ -> failwith "Mixed types in int list") values)
        | (FloatVal _)::_ ->
            FloatList (List.map (function FloatVal f -> f | _ -> failwith "Mixed types in float list") values)
        | _ -> failwith "Unsupported list type")
    | Index (list_expr, index_expr) ->
        let list_val = eval_expr list_expr in
        let index_val = eval_expr index_expr in
        (match (list_val, index_val) with
        | IntList xs, IntVal i ->
            if i < 0 || i >= List.length xs then failwith "Index out of bounds"
            else IntVal (List.nth xs i)
        | FloatList xs, IntVal i ->
            if i < 0 || i >= List.length xs then failwith "Index out of bounds"
            else FloatVal (List.nth xs i)
        | _ -> failwith "Invalid list indexing")
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

    | Binop (e1, Leq, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> IntVal (if i1 <= i2 then 1 else 0)
        | FloatVal f1, FloatVal f2 -> IntVal (if f1 <= f2 then 1 else 0)
        | IntVal i, FloatVal f -> IntVal (if float_of_int i <= f then 1 else 0)
        | FloatVal f, IntVal i -> IntVal (if f <= float_of_int i then 1 else 0)
        | _ -> failwith "Type error in '<=' comparison")

    | Binop (e1, Geq, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> IntVal (if i1 >= i2 then 1 else 0)
        | FloatVal f1, FloatVal f2 -> IntVal (if f1 >= f2 then 1 else 0)
        | IntVal i, FloatVal f -> IntVal (if float_of_int i >= f then 1 else 0)
        | FloatVal f, IntVal i -> IntVal (if f >= float_of_int i then 1 else 0)
        | _ -> failwith "Type error in '>=' comparison")

    | Binop (e1, And, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | BoolVal b1, BoolVal b2 -> BoolVal (b1 && b2)
        | _ -> failwith "Type error: '&&' requires boolean operands")

    | Binop (e1, Or, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | BoolVal b1, BoolVal b2 -> BoolVal (b1 || b2)
        | _ -> failwith "Type error: '||' requires boolean operands")

    | Unop (Not, e) ->
        let v = eval_expr e in
        (match v with
        | BoolVal b -> BoolVal (not b)
        | _ -> failwith "Type error: Not requires a boolean")

    | Transform(list_expr, from_expr, to_expr) ->
        let list_val = eval_expr list_expr in
        let from_val = eval_expr from_expr in
        let to_val = eval_expr to_expr in
        (match (list_val, from_val, to_val) with
        | IntList lst, StringVal "WGS84", StringVal "UTM" ->
            IntList (transform_list lst "WGS84" "UTM")
        | IntList lst, StringVal "UTM", StringVal "WGS84" ->
            IntList (transform_list lst "UTM" "WGS84")
        | _ -> failwith "Invalid arguments for transform")

    | Sort (list_expr, ascending_opt) ->
        let list_val = eval_expr list_expr in
        let ascending = match ascending_opt with
            | Some b -> b
            | None -> true
        in
        match list_val with
        | IntList lst ->
            let sorted = List.sort compare lst in
            IntList (if ascending then sorted else List.rev sorted)
        | FloatList lst ->
            let sorted = List.sort compare lst in
            FloatList (if ascending then sorted else List.rev sorted)
        | _ -> failwith "Sort only supports lists containing elements of the same type, integers or floats."

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
        let s = if f = float_of_int (int_of_float f) then
            (string_of_int (int_of_float f) ^ ".0")
          else
            string_of_float f
        in
        print_endline s
      | StringVal s ->
          print_endline s
      | BoolVal b -> print_endline (string_of_bool b)
      | IntList iL ->
          let s = "[" ^ String.concat ", " (List.map string_of_int iL) ^ "]" in
          print_endline s
      | FloatList fL ->
          let s = "[" ^ String.concat ", " (List.map string_of_float fL) ^ "]" in
          print_endline s
      | IntTuple iT ->  (* Handle IntTuple *)
        let s = "(" ^ String.concat ", " (List.map string_of_int iT) ^ ")" in
        print_endline s
      | FloatTuple fT ->  (* Handle FloatTuple *)
        let s = "(" ^ String.concat ", " (List.map string_of_float fT) ^ ")" in
        print_endline s
      )
  | If (cond_expr, then_branch, else_opt) ->
      (match eval_expr cond_expr with
      | IntVal v when v <> 0 -> List.iter eval_stmt then_branch
      | IntVal 0 -> (match else_opt with Some else_branch -> List.iter eval_stmt else_branch | None -> ())
      | BoolVal true -> List.iter eval_stmt then_branch
      | BoolVal false -> (match else_opt with Some else_branch -> List.iter eval_stmt else_branch | None -> ())
      | _ -> failwith "Condition must evaluate to an integer or boolean")
  | ListAssign (name, index_expr, value_expr) ->
        let list_val = Env.find name !env in
        let index_val = eval_expr index_expr in
        let value = eval_expr value_expr in
        (match (list_val, index_val) with
        | IntList xs, IntVal i ->
            if i < 0 || i >= List.length xs then failwith "Index out of bounds"
            else
                let updated = List.mapi (fun idx v -> if idx = i then (match value with IntVal v' -> v' | _ -> failwith "Expected int value") else v) xs in
                env := Env.add name (IntList updated) !env
        | FloatList xs, IntVal i ->
            if i < 0 || i >= List.length xs then failwith "Index out of bounds"
            else
                let updated = List.mapi (fun idx v -> if idx = i then (match value with FloatVal v' -> v' | _ -> failwith "Expected float value") else v) xs in
                env := Env.add name (FloatList updated) !env
        | _ -> failwith "Invalid list assignment")
  | _ ->
      failwith "unimplemented"

let run (prog: program) =
  List.iter eval_stmt prog
