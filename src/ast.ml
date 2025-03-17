type identifier = string

type primitive =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string

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
