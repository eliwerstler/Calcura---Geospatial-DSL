(* test.ml *)
open Scanner
open Parser
open Interpreter

let test input =
  print_endline ("Test: " ^ input);
  try
    let tokens = tokenize input in
    let ast = parse tokens in
    run ast;
    print_endline "Passed."
  with e ->
    print_endline ("Failed: " ^ Printexc.to_string e)

(* TODO: Add comprehensive test cases *)
let () =
  test "let x = 5;";
  test "print(3 + 4);"
