(* lexer.ml *)

(* TODO: Implement all tokens  *)
type token =
  | INT of int
  | FLOAT of float
  | BOOL of bool
  | STRING of string
  | CHAR of char
  | PLUS | MINUS | TIMES | DIV | MOD
  | ASSIGN | EQ | NEQ
  | AND | OR | NOT
  | LPAREN | RPAREN
  | SEMI | COMMA

exception LexError of string

(* TODO: Implement the tokenize function to convert input string into tokens *)

let tokenize (input: string) : token list = []
