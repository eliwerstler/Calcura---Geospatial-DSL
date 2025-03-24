(* main.ml *)
open Scanner
open Parser
open Interpreter

(* Input -> Scanner -> Parser -> AST -> Interpreter -> Output *)

let () =
  let input = read_line () in
  try
    let tokens = tokenize input in
    let ast = parse tokens in
    run ast
  with
  | LexError msg -> prerr_endline ("Lexer Error: "^msg)
  | ParseError msg -> prerr_endline ("Parser Error: "^msg)
