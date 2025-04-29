type identifier = string

type primitive =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string  (* Add StringLit for string literals *)
  | IntListLit of int list
  | FloatListLit of float list

type typ =
  | Int | Float | Bool | Char | String
  | List of typ

type expr =
  | Literal of primitive
  | Id of identifier
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | ListExpr of expr list
  | Call of identifier * expr list
  | IntListLit of int list
  | FloatListLit of float list
  | Index of expr * expr
  | Transform of expr * expr * expr
  | Sort of expr * bool option


and binop = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or
and unop = Neg | Not

type stmt =
  | Decl of identifier * typ option * expr
  | Assign of identifier * expr
  | Expr of expr
  | If of expr * stmt list * stmt list option
  | For of identifier * expr * stmt list
  | While of expr * stmt list
  | Return of expr option
  | Print of expr

type program = stmt list

let rec string_of_expr = function
  | Literal (IntLit i) -> string_of_int i
  | Literal (FloatLit f) -> string_of_float f
  | Literal (BoolLit b) -> string_of_bool b
  | Literal (CharLit c) -> "'" ^ String.make 1 c ^ "'"
  | Literal (StringLit s) -> "\"" ^ s ^ "\""
  | Literal (IntListLit lst) -> "[" ^ String.concat ", " (List.map string_of_int lst) ^ "]"
  | Literal (FloatListLit lst) -> "[" ^ String.concat ", " (List.map string_of_float lst) ^ "]"
  | Id id -> id
  | Binop (e1, op, e2) ->
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
        | Equal -> "==" | Neq -> "!=" | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
        | And -> "&&" | Or -> "||"
      in
      "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"
  | Unop (op, e) ->
      let op_str = match op with
        | Neg -> "-" | Not -> "!"
      in
      "(" ^ op_str ^ string_of_expr e ^ ")"
  | ListExpr lst -> "[" ^ String.concat ", " (List.map string_of_expr lst) ^ "]"
  | Call (id, args) -> id ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Transform (list_expr, from_expr, to_expr) ->
      "transform(" ^ string_of_expr list_expr ^ ", from=" ^ string_of_expr from_expr ^ ", to=" ^ string_of_expr to_expr ^ ")"
  | Index (list_expr, index_expr) ->
      string_of_expr list_expr ^ "[" ^ string_of_expr index_expr ^ "]"
  | _ -> "Unknown expression"



  let rec string_of_stmt = function
  | Decl (id, _, expr) -> "let " ^ id ^ " = " ^ string_of_expr expr ^ ";"
  | Assign (id, expr) -> id ^ " = " ^ string_of_expr expr ^ ";"
  | Expr expr -> string_of_expr expr ^ ";"
  | Print expr -> "print(" ^ string_of_expr expr ^ ");"
  | If (cond, then_branch, else_branch) ->
      let else_str = match else_branch with
        | Some stmts -> " else " ^ string_of_block stmts
        | None -> ""
      in
      "if (" ^ string_of_expr cond ^ ") " ^ string_of_block then_branch ^ else_str
  | For (id, expr, stmts) ->
      "for (" ^ id ^ " in " ^ string_of_expr expr ^ ") " ^ string_of_block stmts
  | While (cond, stmts) ->
      "while (" ^ string_of_expr cond ^ ") " ^ string_of_block stmts
  | Return None -> "return;"
  | Return (Some expr) -> "return " ^ string_of_expr expr ^ ";"

and string_of_block stmts =
  "{ " ^ String.concat " " (List.map string_of_stmt stmts) ^ " }"


  let string_of_program stmts =
    String.concat "\n" (List.map string_of_stmt stmts)