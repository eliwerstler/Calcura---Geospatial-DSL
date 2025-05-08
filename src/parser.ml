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
    | MINUS :: INT i :: rest ->
        parse_binop (Literal (IntLit (-i))) rest
    | MINUS :: FLOAT f :: rest ->
        parse_binop (Literal (FloatLit (-.f))) rest
    | STRING s :: rest ->
        parse_binop (Literal (StringLit s)) rest
    | ID fn :: LPAREN :: rest ->
        let rec collect_args toks acc =
            match toks with
            | RPAREN :: rest' -> (List.rev acc, rest')
            | _ ->
                let (arg, rest1) = parse_expr toks in
                let rest2 = match rest1 with
                            | COMMA :: rs -> rs
                            | _ -> rest1
                in
                collect_args rest2 (arg::acc)
            in
            let (args, rest') = collect_args rest [] in
            parse_binop (Call(fn, args)) rest'
    | ID id :: rest ->
        parse_binop (Id id) rest
    | BOOL b :: rest ->
        parse_binop (Literal (BoolLit b)) rest
    | NOT :: rest ->
        let (expr, rest') = parse_expr rest in
        let result = Unop (Not, expr) in
        parse_binop result rest'
    | WGS84 :: rest ->
        parse_binop (Literal (StringLit "WGS84")) rest
    | UTM :: rest ->
        parse_binop (Literal (StringLit "UTM")) rest
    | LBRACKET :: RBRACKET :: rest' ->
        parse_binop (Literal (IntListLit [])) rest'
    | LBRACKET :: rest ->
        let rec collect_elements toks acc =
                match toks with
                | RBRACKET :: rest' ->
                    (match acc with
                    | (INT _ :: _) ->
                      Literal (IntListLit (List.rev_map (function INT i -> i | _ -> failwith "Expected int") acc)), rest'
                    | (FLOAT _ :: _) ->
                      Literal (FloatListLit (List.rev_map (function FLOAT f -> f | _ -> failwith "Expected float") acc)), rest'
                    | _ -> raise (ParseError "Empty or mixed-type list not yet supported"))
                | INT i :: COMMA :: rest' -> collect_elements rest' (INT i :: acc)
                | INT i :: rest' -> collect_elements rest' (INT i :: acc)
                | FLOAT f :: COMMA :: rest' -> collect_elements rest' (FLOAT f :: acc)
                | FLOAT f :: rest' -> collect_elements rest' (FLOAT f :: acc)
                | MINUS :: INT i :: COMMA :: rest' -> collect_elements rest' (INT (-i) :: acc)
                | MINUS :: INT i :: rest' -> collect_elements rest' (INT (-i) :: acc)
                | MINUS :: FLOAT f :: COMMA :: rest' -> collect_elements rest' (FLOAT (-. f) :: acc)
                | MINUS :: FLOAT f :: rest' -> collect_elements rest' (FLOAT (-. f) :: acc)
                | _ -> raise (ParseError "Malformed list literal")
        in
        let (literal_expr, rest') = collect_elements rest [] in
        parse_binop literal_expr rest'
    | LPAREN :: rest ->
        let rec parse_tuple_elements toks acc =
            match toks with
            | RPAREN :: rest' -> (List.rev acc, rest') (* End of tuple *)
            | COMMA :: rest' -> parse_tuple_elements rest' acc (* Skip commas *)
            | _ ->
                let (expr, rest') = parse_expr toks in
                parse_tuple_elements rest' (expr :: acc)
        in
        let (elements, rest') = parse_tuple_elements rest [] in
        (match elements with
        | [] -> raise (ParseError "Empty tuple is not allowed")
        | (Literal (IntLit _) :: _) when List.for_all (function Literal (IntLit _) -> true | _ -> false) elements ->
            parse_binop (Literal (IntTupleLit (List.map (function Literal (IntLit i) -> i | _ -> failwith "Expected int") elements))) rest'
        | (Literal (FloatLit _) :: _) when List.for_all (function Literal (FloatLit _) -> true | _ -> false) elements ->
            parse_binop (Literal (FloatTupleLit (List.map (function Literal (FloatLit f) -> f | _ -> failwith "Expected float") elements))) rest'
        | _ -> raise (ParseError "Mixed-type tuples are not supported"))
    (* TODO:
       - Character literals
       - Function calls
       - List operations (shift_left, shift_right, swap, etc.)
    *)
    | MAX :: LPAREN :: rest ->
        let (list_expr, rest1) = parse_expr rest in
        (match rest1 with
        | RPAREN :: rest2 -> parse_binop (Call("max", [list_expr])) rest2
        | _ -> raise (ParseError "Expected ')'"))
    | MIN :: LPAREN :: rest ->
        let (list_expr, rest1) = parse_expr rest in
        (match rest1 with
        | RPAREN :: rest2 -> parse_binop (Call("min", [list_expr])) rest2
        | _ -> raise (ParseError "Expected ')'"))

    | TRANSFORM :: LPAREN :: rest ->
        (* Parse the list expression *)
        let (list_expr, rest1) = parse_expr rest in
        (match list_expr with
        | Literal (FloatTupleLit [x1; x2]) -> (* Literal float tuple *)
            (match rest1 with
            | COMMA :: FROM :: ASSIGN :: rest2 ->
                (* Parse the 'from' argument *)
                let (from_expr, rest3) = parse_expr rest2 in
                (match from_expr with
                | Literal (StringLit "WGS84") | Literal (StringLit "UTM") -> (* Ensure it's a valid coordinate system *)
                    (match rest3 with
                    | COMMA :: TO :: ASSIGN :: rest4 ->
                        (* Parse the 'to' argument *)
                        let (to_expr, rest5) = parse_expr rest4 in
                        (match to_expr with
                        | Literal (StringLit "WGS84") | Literal (StringLit "UTM") -> (* Ensure it's a valid coordinate system *)
                            (match rest5 with
                            | RPAREN :: rest6 ->
                                let transform_expr = Transform (list_expr, from_expr, to_expr) in
                                print_endline (Ast.string_of_expr transform_expr); (* Debugging output *)
                                (transform_expr, rest6)
                            | _ -> raise (ParseError "Expected closing parenthesis after transform arguments"))
                        | _ -> raise (ParseError "Expected 'to' to be either 'WGS84' or 'UTM'"))
                    | _ -> raise (ParseError "Expected 'to=' argument in transform"))
                | _ -> raise (ParseError "Expected 'from' to be either 'WGS84' or 'UTM'"))
            | _ -> raise (ParseError "Expected 'from=' argument in transform"))
        | Id _ -> (* Identifier that could resolve to a float tuple *)
            (match rest1 with
            | COMMA :: FROM :: ASSIGN :: rest2 ->
                (* Parse the 'from' argument *)
                let (from_expr, rest3) = parse_expr rest2 in
                (match from_expr with
                | Literal (StringLit "WGS84") | Literal (StringLit "UTM") -> (* Ensure it's a valid coordinate system *)
                    (match rest3 with
                    | COMMA :: TO :: ASSIGN :: rest4 ->
                        (* Parse the 'to' argument *)
                        let (to_expr, rest5) = parse_expr rest4 in
                        (match to_expr with
                        | Literal (StringLit "WGS84") | Literal (StringLit "UTM") -> (* Ensure it's a valid coordinate system *)
                            (match rest5 with
                            | RPAREN :: rest6 ->
                                let transform_expr = Transform (list_expr, from_expr, to_expr) in
                                print_endline (Ast.string_of_expr transform_expr); (* Debugging output *)
                                (transform_expr, rest6)
                            | _ -> raise (ParseError "Expected closing parenthesis after transform arguments"))
                        | _ -> raise (ParseError "Expected 'to' to be either 'WGS84' or 'UTM'"))
                    | _ -> raise (ParseError "Expected 'to=' argument in transform"))
                | _ -> raise (ParseError "Expected 'from' to be either 'WGS84' or 'UTM'"))
            | _ -> raise (ParseError "Expected 'from=' argument in transform"))
        | _ -> raise (ParseError "Expected list_expr to be a FloatTuple or an Id"))

    | SORT :: LPAREN :: rest ->
        let (list_expr, rest1) = parse_expr rest in
        (match rest1 with
        | COMMA :: BOOL ascending :: RPAREN :: rest2 ->
            parse_binop (Sort (list_expr, Some ascending)) rest2
        | RPAREN :: rest2 -> (* Default to ascending if no bool is provided *)
            parse_binop (Sort (list_expr, None)) rest2
        | _ -> raise (ParseError "Expected closing parenthesis ')' after sort arguments"))

    | AVG :: LPAREN :: rest ->
        let (list_expr, rest') = parse_expr rest in
        (match rest' with
        | RPAREN :: rest'' -> parse_binop (Avg list_expr) rest''
        | _ -> raise (ParseError "Expected closing parenthesis ')' after avg argument"))

    | MEDIAN :: LPAREN :: rest ->
        let (list_expr, rest') = parse_expr rest in
        (match rest' with
        | RPAREN :: rest'' -> parse_binop (Median list_expr) rest''
        | _ -> raise (ParseError "Expected closing parenthesis ')' after median argument"))

    | SHIFT_LEFT :: LPAREN :: rest ->
        let (list_expr, rest1) = parse_expr rest in
        (match rest1 with
        | COMMA :: rest2 ->
            let (n_expr, rest3) = parse_expr rest2 in
            (match rest3 with RPAREN :: rest'' ->
                parse_binop (ShiftLeft (list_expr, n_expr)) rest''
            | _ -> raise (ParseError "Expected ')' after shift_left arguments"))
        | _ -> raise (ParseError "Expected ',' in shift_left"))

    | SHIFT_RIGHT :: LPAREN :: rest ->
        let (list_expr, rest1) = parse_expr rest in
        (match rest1 with
        | COMMA :: rest2 ->
            let (n_expr, rest3) = parse_expr rest2 in
            (match rest3 with RPAREN :: rest'' ->
                parse_binop (ShiftRight (list_expr, n_expr)) rest''
            | _ -> raise (ParseError "Expected ')' after shift_right args"))
        | _ -> raise (ParseError "Expected ',' in shift_right"))

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
    | GT :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Greater, right)) rest'
    | LT :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Less, right)) rest'
    | LEQ :: rest ->
        let (right, rest') = parse_expr rest in
        parse_binop (Binop (left, Leq, right)) rest'
    | GEQ :: rest ->
        let (right, rest') = parse_expr rest in
        parse_binop (Binop (left, Geq, right)) rest'
    | EQ :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Equal, right)) rest'
    | NEQ :: rest ->
            let (right, rest') = parse_expr rest in
            parse_binop (Binop (left, Neq, right)) rest'
    | AND :: rest ->
        let (right, rest') = parse_expr rest in
        parse_binop (Binop (left, And, right)) rest'
    | OR :: rest ->
        let (right, rest') = parse_expr rest in
        parse_binop (Binop (left, Or, right)) rest'
    | LBRACKET :: rest ->
            let (index_expr, rest') = parse_expr rest in
            (match rest' with
                | RBRACKET :: rest'' ->
                parse_binop (Index (left, index_expr)) rest''
                | _ -> raise (ParseError "Expected ']' after index"))
    (* TODO:
       - Type casting with 'as'
    *)
    | _ -> (left, tokens)
let rec parse_block tokens =
        match tokens with
        | LBRACE :: rest ->
                let rec parse_statements toks acc =
                match toks with
                | RBRACE :: rest' -> (List.rev acc, rest')
                | _ ->
                        let (stmt, rest') = parse_stmt toks in
                        parse_statements rest' (stmt :: acc)
                in
                parse_statements rest []
        | _ -> raise (ParseError "Expected '{' to start a block")
and parse_stmt tokens =
    match tokens with
    | FUNC :: ID name :: LPAREN :: rest1 ->
        let rec parse_params toks acc =
            match toks with
            | RPAREN :: rest -> (List.rev acc, rest)
            | ID p :: COMMA :: rest' -> parse_params rest' (p :: acc)
            | ID p :: rest'       -> parse_params rest' (p :: acc)
            | _ -> raise (ParseError "Malformed function parameters")
          in
          let (params, rest2) = parse_params rest1 [] in
          let (body, rest3) = parse_block rest2 in
          (FuncDef (name, params, body), rest3)
    | LET :: ID id :: ASSIGN :: rest ->
            let (expr, rest') = parse_expr rest in
            (match rest' with
            | SEMI :: rest'' -> (Decl (id, None, expr), rest'')
            | _ -> raise (ParseError "Expected semicolon after declaration"))
    | RETURN :: rest ->
        (match rest with
         | SEMI :: rest' -> (Return None, rest')
         | _ ->
             let (expr_node, rest') = parse_expr rest in
             (match rest' with
              | SEMI :: rest'' -> (Return (Some expr_node), rest'')
              | _ -> raise (ParseError "Expected semicolon after return")))
    | IF :: LPAREN :: rest ->
        let (cond_expr, rest1) = parse_expr rest in
        (match rest1 with
        | RPAREN :: rest2 ->
                let (then_branch, rest3) = parse_block rest2 in
                (match rest3 with
                | ELSE :: rest4 ->
                let (else_branch, rest5) = parse_block rest4 in
                (If (cond_expr, then_branch, Some else_branch), rest5)
                | _ ->
                (If (cond_expr, then_branch, None), rest3))
        | _ -> raise (ParseError "Expected ')' after condition"))
    | ID id :: LBRACKET :: rest ->
        let (index_expr, rest1) = parse_expr rest in
        (match rest1 with
        | RBRACKET :: ASSIGN :: rest2 ->
                let (value_expr, rest3) = parse_expr rest2 in
                (match rest3 with
                | SEMI :: rest4 -> (ListAssign(id, index_expr, value_expr), rest4)
                | _ -> raise (ParseError "Expected ';' after list assignment"))
        | _ -> raise (ParseError "Expected ']' after index"))
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
