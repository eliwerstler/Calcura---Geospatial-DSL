(* main.ml *)
open Lexer
open Parser
open Interpreter

let () =
  let input = read_line () in
  try
    let tokens = tokenize input in
    let ast = parse tokens in
    run ast
  with
  | LexError msg -> prerr_endline ("Lexer Error: "^msg)
  | ParseError msg -> prerr_endline ("Parser Error: "^msg)
