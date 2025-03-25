(* parser.ml *)
open Ast
open Scanner

exception ParseError of string

(* TODO:
   1. Function declarations and calls
   2. List literals and operations
   3. Control flow statements (if, else, for, while)
   4. Type annotations
   5. Variable declarations with explicit types
   6. All binary and unary operators from spec
   7. Block expressions
*)

let rec parse_expr tokens =
    match tokens with
    | INT i :: rest ->
            parse_binop (Literal (IntLit i)) rest
    | FLOAT f :: rest ->
            parse_binop (Literal (FloatLit f)) rest
    | STRING s :: rest ->
            parse_binop (Literal (StringLit s)) rest
    | ID id :: rest ->
            parse_binop (Id id) rest
    (* TODO:
       - Boolean literals (true/false)
       - Character literals
       - List literals [1, 2, 3]
       - Function calls
       - List operations (shift_left, shift_right, swap, etc.)
    *)
    | LPAREN :: rest ->
            let (expr, rest') = parse_expr rest in
            (match rest' with
            | RPAREN :: rest'' ->
                    parse_binop expr rest''
            | _ -> raise (ParseError "Expected closing parenthesis"))
    | _ -> raise (ParseError "Unexpected token in expression")

and parse_binop left tokens =
    match tokens with
    | PLUS :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Add, right)) rest'
    | MINUS :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Sub, right)) rest'
    | TIMES :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Mult, right)) rest'
    | DIV :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Div, right)) rest'
    | MOD :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Mod, right)) rest'
    (* TODO: 
       - Comparison operators (==, !=, <, >, <=, >=)
       - Logical operators (and, or)
       - Type casting with 'as'
    *)
    | _ -> (left, tokens)

let rec parse_stmt tokens =
    match tokens with
    | LET :: ID id :: ASSIGN :: rest ->
            let (expr, rest') = parse_expr rest in
            (match rest' with
            | SEMI :: rest'' -> (Decl (id, None, expr), rest'')
            | _ -> raise (ParseError "Expected semicolon after declaration"))
    (* TODO: 
       - Variable declarations with explicit types
       - Function declarations
       - If/else statements
       - For loops
       - While loops
       - Return statements
       - Block statements
       - Break and continue
    *)
    | PRINT :: LPAREN :: rest ->
            (try
                let (expr, rest') = parse_expr rest in
                (match rest' with
                | RPAREN :: SEMI :: rest'' -> (Print expr, rest'')
                | RPAREN :: _ -> raise (ParseError "Expected semicolon after print statement")
                | _ -> raise (ParseError "Expected closing parenthesis"))
            with 
            | ParseError _ -> (Print (Literal (StringLit "Error parsing print argument")), []))
    | _ ->
            (try
                let (expr, rest') = parse_expr tokens in
                (match rest' with
                | SEMI :: rest'' -> (Expr expr, rest'')
                | _ -> raise (ParseError "Expected semicolon after expression"))
            with
            | ParseError _ -> raise (ParseError "Invalid statement"))

(* TODO: 
   - Parsing list operations
   - Parsing built-in functions (max, min, avg, median)
   - Parsing function parameters and return types
*)

let rec parse_program tokens =
    if tokens = [EOF] then []
    else
        match tokens with
        | [] -> []
        | EOF :: _ -> []
        | _ ->
                try
                    let (stmt, rest) = parse_stmt tokens in
                    stmt :: parse_program rest
                with
                | ParseError msg as e ->
                    Printf.printf "Parse error: %s\n" msg;
                    if tokens <> [EOF] && List.hd tokens <> EOF then
                        parse_program (List.tl tokens)
                    else
                        raise e

let parse tokens =
    parse_program tokens
