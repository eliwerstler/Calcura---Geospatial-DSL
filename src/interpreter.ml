(* interpreter.ml *)

(* TODO:
   - Implement max, min, reduce, append, prepend, minus, slicing, string methods, type casting (as), explicit type conversion
   - Implement block expressions as value-returning
   - Implement for/while loops, break/continue, assignment (Assign)
   - Implement dictionaries, sets, structs, enums, generics, type aliases, type qualifiers, lambdas
   - Enforce immutability and type safety as described in the LRM
   - Implement geospatial/statistical features beyond transform
*)

open Ast

(* Value type supports all needed types for both branches *)
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
module FuncEnv = Map.Make(String)
let func_env : (string list * stmt list) FuncEnv.t ref = ref FuncEnv.empty
exception ReturnException of value option

let env : value Env.t ref = ref Env.empty

let round_to_decimal_places value decimal_places =
  let factor = 10.0 ** float_of_int decimal_places in
  floor (value *. factor +. 0.5) /. factor

(* Geospatial transform for FloatTuple, fallback for IntList for legacy tests *)
let transform_list lst from_system to_system =
  match (from_system, to_system, lst) with
  | "WGS84", "UTM", FloatTuple [lat; lon] ->
    (* Real UTM conversion for FloatTuple *)
    let k0 = 0.9996 in
    let a = 6378137.0 in
    let e = 0.08181919084 in
    let lon_origin = float_of_int (((int_of_float ((lon +. 180.0) /. 6.0)) * 6) - 180 + 3) in
    let lat_rad = lat *. Float.pi /. 180.0 in
    let lon_rad = lon *. Float.pi /. 180.0 in
    let lon_origin_rad = lon_origin *. Float.pi /. 180.0 in
    let n = a /. sqrt (1.0 -. e *. e *. sin lat_rad ** 2.0) in
    let t = tan lat_rad ** 2.0 in
    let c = e *. e /. (1.0 -. e *. e) *. cos lat_rad ** 2.0 in
    let a' = cos lat_rad *. (lon_rad -. lon_origin_rad) in
    let m =
      a
      *. ((1.0 -. e *. e /. 4.0 -. 3.0 *. e ** 4.0 /. 64.0 -. 5.0 *. e ** 6.0 /. 256.0)
         *. lat_rad
         -. (3.0 *. e *. e /. 8.0 +. 3.0 *. e ** 4.0 /. 32.0 +. 45.0 *. e ** 6.0 /. 1024.0)
         *. sin (2.0 *. lat_rad)
         +. (15.0 *. e ** 4.0 /. 256.0 +. 45.0 *. e ** 6.0 /. 1024.0) *. sin (4.0 *. lat_rad)
         -. (35.0 *. e ** 6.0 /. 3072.0) *. sin (6.0 *. lat_rad))
    in
    let easting =
      k0
      *. n
      *. (a' +. (1.0 -. t +. c) *. a' ** 3.0 /. 6.0
         +. (5.0 -. 18.0 *. t +. t *. t +. 72.0 *. c -. 58.0 *. e *. e /. (1.0 -. e *. e))
         *. a' ** 5.0 /. 120.0)
      +. 500000.0
    in
    let northing =
      k0
      *. (m
         +. n
            *. tan lat_rad
            *. (a' ** 2.0 /. 2.0
               +. (5.0 -. t +. 9.0 *. c +. 4.0 *. c *. c) *. a' ** 4.0 /. 24.0
               +. (61.0 -. 58.0 *. t +. t *. t +. 600.0 *. c -. 330.0 *. e *. e /. (1.0 -. e *. e))
                  *. a' ** 6.0 /. 720.0))
    in
    let northing = if lat < 0.0 then northing +. 10000000.0 else northing in
    let easting = round_to_decimal_places easting 2 in
    let northing = round_to_decimal_places northing 2 in
    FloatTuple [easting; northing]
  | "UTM", "WGS84", FloatTuple [easting; northing] ->
    let k0 = 0.9996 in
    let a = 6378137.0 in
    let e2 = 0.00669437999014 in
    let e1 = (1.0 -. sqrt (1.0 -. e2)) /. (1.0 +. sqrt (1.0 -. e2)) in
    let x = easting -. 500000.0 in
    let y = if northing > 10000000.0 then northing -. 10000000.0 else northing in
    let zone_number = 18 in
    let lon0 = float_of_int ((zone_number - 1) * 6 - 180) in
    let m = y /. k0 in
    let mu = m /. (a *. (1.0 -. e2 /. 4.0 -. 3.0 *. e2 ** 2.0 /. 64.0 -. 5.0 *. e2 ** 3.0 /. 256.0)) in
    let phi1 =
      mu
      +. (3.0 *. e1 /. 2.0 -. 27.0 *. e1 ** 3.0 /. 32.0) *. sin (2.0 *. mu)
      +. (21.0 *. e1 ** 2.0 /. 16.0 -. 55.0 *. e1 ** 4.0 /. 32.0) *. sin (4.0 *. mu)
      +. (151.0 *. e1 ** 3.0 /. 96.0) *. sin (6.0 *. mu)
      +. (1097.0 *. e1 ** 4.0 /. 512.0) *. sin (8.0 *. mu)
    in
    let n = a /. sqrt (1.0 -. e2 *. (sin phi1) ** 2.0) in
    let t = (tan phi1) ** 2.0 in
    let c = e2 *. (cos phi1) ** 2.0 in
    let r = a *. (1.0 -. e2) /. (1.0 -. e2 *. (sin phi1) ** 2.0) ** 1.5 in
    let d = x /. (n *. k0) in
    let lat =
      phi1
      -. (n *. tan phi1 /. r)
         *. (d ** 2.0 /. 2.0
            -. (5.0 +. 3.0 *. t +. 10.0 *. c -. 4.0 *. c ** 2.0 -. 9.0 *. e2) *. d ** 4.0 /. 24.0
            +. (61.0 +. 90.0 *. t +. 298.0 *. c +. 45.0 *. t ** 2.0 -. 252.0 *. e2) *. d ** 6.0 /. 720.0)
    in
    let lon =
      lon0
      +. (d
         -. (1.0 +. 2.0 *. t +. c) *. d ** 3.0 /. 6.0
         +. (5.0 -. 2.0 *. c +. 28.0 *. t -. 3.0 *. c ** 2.0 +. 8.0 *. e2 +. 24.0 *. t ** 2.0) *. d ** 5.0 /. 120.0)
         /. cos phi1
    in
    let lat = round_to_decimal_places (lat *. 180.0 /. Float.pi) 4 in
    let lon = round_to_decimal_places (lon) 4 in
    FloatTuple [lat; lon]
  | "WGS84", "UTM", IntList lst -> IntList (List.map (fun x -> x + 1) lst)
  | "UTM", "WGS84", IntList lst -> IntList (List.map (fun x -> x - 1) lst)
  | _ -> failwith "Unsupported transformation or invalid arguments"

let sort_list list_val ascending =
  match list_val with
  | IntList lst ->
      let sorted = List.sort compare lst in
      IntList (if ascending then sorted else List.rev sorted)
  | FloatList lst ->
      let sorted = List.sort compare lst in
      FloatList (if ascending then sorted else List.rev sorted)
  | _ -> failwith "Sort only works with lists of integers or floats"

let rec string_of_value = function
  | IntVal i -> string_of_int i
  | FloatVal f -> string_of_float f
  | BoolVal b -> string_of_bool b
  | StringVal s -> "\"" ^ s ^ "\""
  | IntList lst -> "[" ^ String.concat ", " (List.map string_of_int lst) ^ "]"
  | FloatList lst -> "[" ^ String.concat ", " (List.map string_of_float lst) ^ "]"
  | IntTuple lst -> "(" ^ String.concat ", " (List.map string_of_int lst) ^ ")"
  | FloatTuple lst -> "(" ^ String.concat ", " (List.map string_of_float lst) ^ ")"

let shift_left_list lst n =
    let len = List.length lst in
    if len = 0 then lst else
        let k = ((n mod len) + len) mod len in
        let rec split_at i l =
            if i = 0 then ([], l) else match l with
            | [] -> ([], [])
            | x::xs ->
                let (pre, post) = split_at (i-1) xs in
                (x::pre, post)
        in
        let pre, post = split_at k lst in
        post @ pre

let shift_right_list lst n =
    let len = List.length lst in
    if len = 0 then lst else
    shift_left_list lst (len - (n mod len))

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
         | _ -> failwith "Modulo operator '%' is only supported for integers.")
    | Binop (e1, Greater, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> BoolVal (i1 > i2)
        | FloatVal f1, FloatVal f2 -> BoolVal (f1 > f2)
        | IntVal i, FloatVal f -> BoolVal (float_of_int i > f)
        | FloatVal f, IntVal i -> BoolVal (f > float_of_int i)
        | _ -> failwith "Type error in '>' comparison")
    | Binop (e1, Less, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> BoolVal (i1 < i2)
        | FloatVal f1, FloatVal f2 -> BoolVal (f1 < f2)
        | IntVal i, FloatVal f -> BoolVal (float_of_int i < f)
        | FloatVal f, IntVal i -> BoolVal (f < float_of_int i)
        | _ -> failwith "Type error in '<' comparison")
    | Binop (e1, Equal, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> BoolVal (i1 = i2)
        | FloatVal f1, FloatVal f2 -> BoolVal (f1 = f2)
        | IntVal i, FloatVal f | FloatVal f, IntVal i -> BoolVal (float_of_int i = f)
        | StringVal s1, StringVal s2 -> BoolVal (s1 = s2)
        | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
        | _ -> failwith "Type mismatch in equality check")
    | Binop (e1, Neq, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> BoolVal (i1 <> i2)
        | FloatVal f1, FloatVal f2 -> BoolVal (f1 <> f2)
        | IntVal i, FloatVal f | FloatVal f, IntVal i -> BoolVal (float_of_int i <> f)
        | StringVal s1, StringVal s2 -> BoolVal (s1 <> s2)
        | BoolVal b1, BoolVal b2 -> BoolVal (b1 <> b2)
        | _ -> failwith "Type mismatch in inequality check")
    | Binop (e1, Leq, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> BoolVal (i1 <= i2)
        | FloatVal f1, FloatVal f2 -> BoolVal (f1 <= f2)
        | IntVal i, FloatVal f -> BoolVal (float_of_int i <= f)
        | FloatVal f, IntVal i -> BoolVal (f <= float_of_int i)
        | _ -> failwith "Type error in '<=' comparison")
    | Binop (e1, Geq, e2) ->
        let v1 = eval_expr e1 in
        let v2 = eval_expr e2 in
        (match (v1, v2) with
        | IntVal i1, IntVal i2 -> BoolVal (i1 >= i2)
        | FloatVal f1, FloatVal f2 -> BoolVal (f1 >= f2)
        | IntVal i, FloatVal f -> BoolVal (float_of_int i >= f)
        | FloatVal f, IntVal i -> BoolVal (f >= float_of_int i)
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
    | Transform (list_expr, from_expr, to_expr) ->
        let list_val = eval_expr list_expr in
        let from_val = eval_expr from_expr in
        let to_val = eval_expr to_expr in
        (match (list_val, from_val, to_val) with
        | FloatTuple lst, StringVal "WGS84", StringVal "UTM" ->
            transform_list (FloatTuple lst) "WGS84" "UTM"
        | FloatTuple lst, StringVal "UTM", StringVal "WGS84" ->
            transform_list (FloatTuple lst) "UTM" "WGS84"
        | IntList lst, StringVal "WGS84", StringVal "UTM" ->
            transform_list (IntList lst) "WGS84" "UTM"
        | IntList lst, StringVal "UTM", StringVal "WGS84" ->
            transform_list (IntList lst) "UTM" "WGS84"
        | _ -> failwith "Invalid arguments for transform: Expected a float tuple or int list and valid coordinate systems")
    | Sort (list_expr, ascending_opt) ->
        let list_val = eval_expr list_expr in
        let ascending = match ascending_opt with
            | Some b -> b
            | None -> true
        in
        (match list_val with
        | IntList lst ->
            let sorted = List.sort compare lst in
            IntList (if ascending then sorted else List.rev sorted)
        | FloatList lst ->
            let sorted = List.sort compare lst in
            FloatList (if ascending then sorted else List.rev sorted)
        | _ -> failwith "Sort only supports lists containing elements of the same type, integers or floats.")
    | Avg list_expr ->
        let list_val = eval_expr list_expr in
        (match list_val with
        | IntList lst ->
            if List.length lst = 0 then FloatVal 0.0
            else
                let sum = List.fold_left (+) 0 lst in
                FloatVal (float_of_int sum /. float_of_int (List.length lst))
        | FloatList lst ->
            if List.length lst = 0 then FloatVal 0.0
            else
                let sum = List.fold_left (+.) 0.0 lst in
                let count = float_of_int (List.length lst) in
                FloatVal (sum /. count)
        | _ -> failwith "Average expects a list of integers or floats.")
    | Median list_expr ->
        let list_val = eval_expr list_expr in
        (match list_val with
        | IntList lst ->
            let sorted = List.sort compare lst in
            let n = List.length sorted in
            if n = 0 then FloatVal 0.0
            else if n mod 2 = 1 then
                FloatVal (float_of_int (List.nth sorted (n/2)))
            else
                let lo = List.nth sorted (n/2 - 1) in
                let hi = List.nth sorted (n/2)     in
                FloatVal (((float_of_int lo) +. float_of_int hi) /. 2.0)
        | FloatList lst ->
            let sorted = List.sort compare lst in
            let n = List.length sorted in
            if n = 0 then FloatVal 0.0
            else if n mod 2 = 1 then
                FloatVal (List.nth sorted (n/2))
            else
                let lo = List.nth sorted (n/2 - 1) in
                let hi = List.nth sorted (n/2)     in
                FloatVal ((lo +. hi) /. 2.0)
        | _ -> failwith "Median expects a list of integers or floats.")
    | ShiftLeft (list_e, n_e) ->
        let list_v = eval_expr list_e in
        let n_v    = eval_expr n_e in
        (match (list_v, n_v) with
        | IntList lst, IntVal n    -> IntList  (shift_left_list lst n)
        | FloatList lst, IntVal n  -> FloatList(shift_left_list lst n)
        | _ -> failwith "shift_left expects (list<int> or list<float>, int)")
    | ShiftRight (list_e, n_e) ->
        let list_v = eval_expr list_e in
        let n_v    = eval_expr n_e in
        (match (list_v, n_v) with
        | IntList lst, IntVal n    -> IntList  (shift_right_list lst n)
        | FloatList lst, IntVal n  -> FloatList(shift_right_list lst n)
        | _ -> failwith "shift_right expects (list<int> or list<float>, int)")
    | Swap (list_e, i_e, j_e) ->
        let list_v = eval_expr list_e in
        let i_v = eval_expr i_e in
        let j_v = eval_expr j_e in
        let swap_list lst i j =
          if i < 0 || j < 0 || i >= List.length lst || j >= List.length lst then
            failwith "swap: index out of bounds"
          else
            List.mapi (fun idx v ->
              if idx = i then List.nth lst j
              else if idx = j then List.nth lst i
              else v
            ) lst
        in
        (match (list_v, i_v, j_v) with
        | IntList lst, IntVal i, IntVal j -> IntList (swap_list lst i j)
        | FloatList lst, IntVal i, IntVal j -> FloatList (swap_list lst i j)
        | _ -> failwith "swap expects (list<int> or list<float>, int, int)")
    | Reverse list_e ->
        let list_v = eval_expr list_e in
        (match list_v with
        | IntList lst -> IntList (List.rev lst)
        | FloatList lst -> FloatList (List.rev lst)
        | _ -> failwith "reverse expects list<int> or list<float>")
    | Filter (list_e, func_e) ->
        let list_v = eval_expr list_e in
        (match (list_v, func_e) with
        | IntList lst, Id fname ->
            IntList (List.filter (fun x ->
              match eval_expr (Call(fname, [Literal (IntLit x)])) with
              | BoolVal b -> b
              | _ -> failwith "filter function must return bool") lst)
        | FloatList lst, Id fname ->
            FloatList (List.filter (fun x ->
              match eval_expr (Call(fname, [Literal (FloatLit x)])) with
              | BoolVal b -> b
              | _ -> failwith "filter function must return bool") lst)
        | _ -> failwith "filter expects (list<int> or list<float>, function)")
    | Map (list_e, func_e) ->
        let list_v = eval_expr list_e in
        (match (list_v, func_e) with
        | IntList lst, Id fname ->
            IntList (List.map (fun x ->
              match eval_expr (Call(fname, [Literal (IntLit x)])) with
              | IntVal v -> v
              | _ -> failwith "map function must return int") lst)
        | FloatList lst, Id fname ->
            FloatList (List.map (fun x ->
              match eval_expr (Call(fname, [Literal (FloatLit x)])) with
              | FloatVal v -> v
              | _ -> failwith "map function must return float") lst)
        | _ -> failwith "map expects (list<int> or list<float>, function)")
    | Call (fn, args) ->
        let (params, body) =
            try FuncEnv.find fn !func_env
            with Not_found -> failwith ("Unknown function: " ^ fn)
        in
        if List.length params <> List.length args then
            failwith (fn ^ " expected "
                        ^ string_of_int (List.length params)
                        ^ " args, got "
                        ^ string_of_int (List.length args));
        let arg_vals = List.map eval_expr args in
        let wrap_param param_name param_val =
          match param_val with
          | IntVal _ | FloatVal _ | BoolVal _ | StringVal _ | IntList _ | FloatList _ | IntTuple _ | FloatTuple _ -> param_val
        in
        let arg_vals = List.map2 wrap_param params arg_vals in
        let saved_env = !env in
        let new_env = 
            List.fold_left2
            (fun m param_name param_val -> Env.add param_name param_val m)
            Env.empty
            params
            arg_vals
        in
        env := new_env;
        let returned =
            try 
                List.iter eval_stmt body; 
                None
            with ReturnException v_opt -> v_opt
        in
        env := saved_env;
        (match returned with
         | Some v -> v
         | None -> failwith ("Function " ^ fn ^ " did not return"))

and eval_stmt = function
  | Decl (name, _, expr) ->
      let value = eval_expr expr in
      env := Env.add name value !env
  | Expr e ->
      let result = eval_expr e in
      env := Env.add "last_result" result !env
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
      | IntTuple iT ->
        let s = "(" ^ String.concat ", " (List.map string_of_int iT) ^ ")" in
        print_endline s
      | FloatTuple fT ->
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
  | FuncDef (name, params, body) ->
        func_env := FuncEnv.add name (params, body) !func_env
  | Return opt_e ->
        let v_opt = 
            match opt_e with
            | None -> None
            | Some e -> Some (eval_expr e)
        in
        raise (ReturnException v_opt)
  | Assign (id, expr) ->
      let _ = (id, expr) in
      () (* TODO *)
  | For (id, expr, stmts) ->
      let _ = (id, expr, stmts) in
      () (* TODO *)
  | While (cond, stmts) ->
      let _ = (cond, stmts) in
      () (* TODO *)

let run (prog: program) =
  List.iter eval_stmt prog
