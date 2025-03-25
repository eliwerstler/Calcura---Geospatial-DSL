open Scanner
open Parser
open Interpreter
open Sys

let read_file filename =
  try
    let ic = open_in_bin filename in
    try
      let content = 
        let size = in_channel_length ic in
        if size = 0 then ""
        else
          let buffer = Bytes.create size in
          really_input ic buffer 0 size;
          Bytes.to_string buffer
      in
      close_in ic;
      content
    with e -> 
      close_in_noerr ic;  (* Close but ignore errors *)
      raise e
  with Sys_error msg -> 
    Printf.printf "Error reading file %s: %s\n" filename msg;
    raise (Sys_error msg)

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

type test_case = {
  filename: string;
  content: string;
  expected_outputs: (int * string) list;
}

let parse_inline_expected_outputs content =
  let lines = String.split_on_char '\n' content in
  let rec find_expected_outputs line_num acc = 
    if line_num >= List.length lines then acc
    else 
      let current_line = List.nth lines line_num in
      let current_line = String.trim current_line in
      if current_line = "" then
        find_expected_outputs (line_num + 1) acc
      else if String.length current_line >= 2 && String.sub current_line 0 2 = "//" then
        if line_num + 1 < List.length lines then
          let next_line = List.nth lines (line_num + 1) in
          if String.starts_with ~prefix:"print" (String.trim next_line) then
            let comment = String.trim (String.sub current_line 2 (String.length current_line - 2)) in
            find_expected_outputs (line_num + 2) (acc @ [(line_num + 1, comment)])
          else
            find_expected_outputs (line_num + 1) acc
        else
          find_expected_outputs (line_num + 1) acc
      else
        find_expected_outputs (line_num + 1) acc
  in
  find_expected_outputs 0 []

let run_and_capture_output ast =
  let buffer = Buffer.create 1024 in
  let old_stdout = Unix.dup Unix.stdout in
  let pipe_out, pipe_in = Unix.pipe () in
  Unix.dup2 pipe_in Unix.stdout;
  Unix.close pipe_in;

  run ast;
  flush stdout;

  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;

  let ic = Unix.in_channel_of_descr pipe_out in
  (try
     while true do
       Buffer.add_channel buffer ic 1024
     done
   with End_of_file -> close_in ic);
  Buffer.contents buffer

let compare_outputs expected_outputs actual_outputs out_channel =
  let rec compare_helper expected actual =
    match (expected, actual) with
    | ([], []) -> ()
    | (expected :: rest_expected, actual :: rest_actual) ->
        if String.trim actual = String.trim expected then
          Printf.fprintf out_channel "PASS: Expected '%s', got '%s'\n" expected actual
        else
          Printf.fprintf out_channel "FAIL: Expected '%s', got '%s'\n" expected actual;
        compare_helper rest_expected rest_actual
    | (expected :: rest_expected, []) ->
        Printf.fprintf out_channel "FAIL: Expected '%s', but got no output\n" expected;
        compare_helper rest_expected []
    | ([], actual :: rest_actual) ->
        Printf.fprintf out_channel "FAIL: Unexpected output '%s'\n" actual;
        compare_helper [] rest_actual
  in
  compare_helper expected_outputs actual_outputs

let rec run_test_suite test_files =
  let output_dir = "test/out" in
  if not (Sys.file_exists output_dir) then
    Unix.mkdir output_dir 0o755;
  
  let output_file = Filename.concat output_dir "test_results.txt" in
  let out_channel = open_out output_file in
  
  let test_cases = 
    List.map (fun filename ->
      let content = read_file filename in
      let expected_outputs = parse_inline_expected_outputs content in
      { filename; content; expected_outputs }
    ) test_files
  in
  
  List.iter (fun test_case ->
    Printf.fprintf out_channel "=== Running test: %s ===\n" test_case.filename;
    
    List.iter (fun (line_num, expected) ->
      Printf.fprintf out_channel "EXPECT: %s\n" expected;
    ) test_case.expected_outputs;
    
    let tokens = tokenize test_case.content in
    let ast = parse tokens in
    
    let actual_output = run_and_capture_output ast in
    let actual_outputs = String.split_on_char '\n' (String.trim actual_output) in

    let expected_outputs = List.map snd test_case.expected_outputs in
    compare_outputs expected_outputs actual_outputs out_channel;

    Printf.fprintf out_channel "\n";
  ) test_cases;
  
  close_out out_channel;
  analyze_test_results output_file
  
and analyze_test_results output_file =
  let lines = String.split_on_char '\n' (read_file output_file) in
  
  let rec check_outputs line_idx passed failed =
    if line_idx >= List.length lines then (passed, failed)
    else
      let current_line = List.nth lines line_idx in
      if String.length current_line >= 5 && String.sub current_line 0 4 = "PASS" then
        check_outputs (line_idx + 1) (passed + 1) failed
      else if String.length current_line >= 5 && String.sub current_line 0 4 = "FAIL" then
        check_outputs (line_idx + 1) passed (failed + 1)
      else
        check_outputs (line_idx + 1) passed failed
  in
  
  let (passed, failed) = check_outputs 0 0 0 in
  Printf.printf "\n=== Test Summary ===\n";
  Printf.printf "Passed: %d, Failed: %d, Total: %d\n" passed failed (passed + failed);
  
  passed = (passed + failed)

let run_all_tests () =
  let test_dir = "test/unit_test" in
  
  (try if not (Sys.file_exists test_dir) then Unix.mkdir test_dir 0o755 with _ -> ());
  
  let test_files = 
    try Array.to_list (Sys.readdir test_dir)
    with _ -> []
  in
  
  let clc_files = List.filter (fun f -> Filename.check_suffix f ".clc") test_files in
  let sorted_files = List.sort String.compare clc_files in
  let full_paths = List.map (fun f -> Filename.concat test_dir f) sorted_files in
  
  let success = run_test_suite full_paths in
  
  if success then exit 0 else exit 1

let () =
  (* single test run *)
  if Array.length Sys.argv > 1 then
    let test_file = Sys.argv.(1) in
    let success = run_test_suite [test_file] in
    exit (if success then 0 else 1)
  else
    run_all_tests ()
