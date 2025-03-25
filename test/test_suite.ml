open Scanner
open Parser
open Interpreter
open Sys

let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let debug_tokens tokens =
  List.iter (fun token ->
    match token with
    | INT i -> Printf.printf "INT(%d) " i
    | STRING s -> Printf.printf "STRING(\"%s\") " s
    | PLUS -> Printf.printf "PLUS "
    | MINUS -> Printf.printf "MINUS "
    | TIMES -> Printf.printf "TIMES "
    | DIV -> Printf.printf "DIV "
    | MOD -> Printf.printf "MOD "
    | LPAREN -> Printf.printf "LPAREN "
    | RPAREN -> Printf.printf "RPAREN "
    | SEMI -> Printf.printf "SEMI "
    | COMMA -> Printf.printf "COMMA "
    | PRINT -> Printf.printf "PRINT "
    | EOF -> Printf.printf "EOF "
    | _ -> Printf.printf "OTHER_TOKEN "
  ) tokens;
  Printf.printf "\n"

let run_test_file filename =
  Printf.printf "\n=== Running test file: %s ===\n" filename;
  try
    let input = read_file filename in
    Printf.printf "File content: '%s'\n" input;
    
    let tokens = tokenize input in
    Printf.printf "Tokens: ";
    debug_tokens tokens;
    
    let ast = parse tokens in
    run ast;
    Printf.printf "Test passed: %s\n" filename
  with
  | Sys_error msg -> Printf.printf "File Error: %s\n" msg
  | LexError msg -> Printf.printf "Lexer Error in file %s: %s\n" filename msg
  | ParseError msg -> Printf.printf "Parser Error in file %s: %s\n" filename msg
  | e -> Printf.printf "Unexpected Error in file %s: %s\n" filename (Printexc.to_string e)

let () =
  let test_dir = "test/unit_test" in
  let test_files = Array.to_list (Sys.readdir test_dir) in
  let clc_files = List.filter (fun f -> Filename.check_suffix f ".clc") test_files in
  List.iter (fun f -> run_test_file (Filename.concat test_dir f)) clc_files
