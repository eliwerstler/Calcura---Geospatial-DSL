(* main.ml *)
open Scanner
open Parser
open Interpreter

let read_file filename =
  let ic = open_in filename in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file ->
      close_in ic;
      acc
  in
  read_all ""

(* Input -> Scanner -> Parser -> AST -> Interpreter -> Output *)

let () =
  if Array.length Sys.argv < 2 then
    prerr_endline "Usage: calcura <file.clc>"
  else
    let filename = Sys.argv.(1) in
    if Filename.check_suffix filename ".clc" then
      try
        let input = read_file filename in  (* Use custom read_file function *)
        let tokens = tokenize input in
        let ast = parse tokens in
        run ast
      with
      | Sys_error msg -> prerr_endline ("File Error: " ^ msg)
      | LexError msg -> prerr_endline ("Lexer Error: " ^ msg)
      | ParseError msg -> prerr_endline ("Parser Error: " ^ msg)
    else
      prerr_endline "Error: Input file must have a .clc extension."
