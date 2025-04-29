(* scanner.ml *)

type token =
  | INT of int
  | FLOAT of float
  | BOOL of bool
  | STRING of string
  | CHAR of char
  | PLUS | MINUS | TIMES | DIV | MOD
  | ASSIGN | EQ | NEQ | LT | LEQ | GT | GEQ
  | AND | OR | NOT
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET
  | SEMI | COMMA | COLON | DOT
  | LET | FUNC | RETURN
  | IF | ELSE | FOR | WHILE | IN | BREAK | CONTINUE
  | PRINT | MAX | MIN | AVG | MEDIAN
  | SHIFT_LEFT | SHIFT_RIGHT | SWAP | REVERSE | SORT | FILTER | MAP
  | INT_TYPE | FLOAT_TYPE | BOOL_TYPE | CHAR_TYPE | STRING_TYPE | LIST
  | TRANSFORM | FROM | TO | WGS84 | UTM
  | AS | ID of string
  | EOF

let string_of_token = function
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | CHAR c -> "CHAR(" ^ String.make 1 c ^ ")"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | ASSIGN -> "ASSIGN"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LEQ -> "LEQ"
  | GT -> "GT"
  | GEQ -> "GEQ"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | SEMI -> "SEMI"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | DOT -> "DOT"
  | LET -> "LET"
  | FUNC -> "FUNC"
  | RETURN -> "RETURN"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | IN -> "IN"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | PRINT -> "PRINT"
  | MAX -> "MAX"
  | MIN -> "MIN"
  | AVG -> "AVG"
  | MEDIAN -> "MEDIAN"
  | SHIFT_LEFT -> "SHIFT_LEFT"
  | SHIFT_RIGHT -> "SHIFT_RIGHT"
  | SWAP -> "SWAP"
  | REVERSE -> "REVERSE"
  | SORT -> "SORT"
  | FILTER -> "FILTER"
  | MAP -> "MAP"
  | INT_TYPE -> "INT_TYPE"
  | FLOAT_TYPE -> "FLOAT_TYPE"
  | BOOL_TYPE -> "BOOL_TYPE"
  | CHAR_TYPE -> "CHAR_TYPE"
  | STRING_TYPE -> "STRING_TYPE"
  | LIST -> "LIST"
  | TRANSFORM -> "TRANSFORM"
  | FROM -> "FROM"
  | TO -> "TO"
  | WGS84 -> "WGS84"
  | UTM -> "UTM"
  | AS -> "AS"
  | ID id -> "ID(" ^ id ^ ")"
  | EOF -> "EOF"

(* TODO:
   - char literals
   - type annotation tokens
   - function declaration/call tokens
   - index (Sayef)
   - sort (Crystal), desc asc
   - special tokens for built-in functions (max, min, avg, median) -> Junqi max min
   - User-defined function (Eli)
   - list operation tokens (shift_left, shift_right, swap, reverse, filter, map)
*)

exception LexError of string

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_alnum c = is_digit c || is_alpha c
let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'


let tokenize (input: string) : token list =
  let length = String.length input in
  let rec tokenize_helper i acc =
    if i >= length then List.rev (EOF :: acc)
    else
      let c = input.[i] in
      if is_whitespace c then tokenize_helper (i + 1) acc
      else if c = '/' && i + 1 < length && input.[i + 1] = '/' then
        skip_comment (i + 2) acc
      else if is_digit c then read_number i acc
      else if is_alpha c || c = '_' then read_identifier i acc
      else read_operator i acc
  and skip_comment i acc =
    if i >= length || input.[i] = '\n' then tokenize_helper (i + 1) acc
    else skip_comment (i + 1) acc
  and read_number i acc =
    let rec collect_digits j =
      if j < length && is_digit input.[j] then collect_digits (j + 1) else j
    in
    let end_int = collect_digits i in
    if end_int < length && input.[end_int] = '.' then
      let end_frac = collect_digits (end_int + 1) in
      let float_str = String.sub input i (end_frac - i) in
      tokenize_helper end_frac (FLOAT (float_of_string float_str) :: acc)
    else
      let int_str = String.sub input i (end_int - i) in
      tokenize_helper end_int (INT (int_of_string int_str) :: acc)

  and read_identifier i acc =
    let rec collect_id j =
      if j < length && (is_alnum input.[j] || input.[j] = '_') then collect_id (j + 1) else j
    in
    let end_id = collect_id i in
    let id_str = String.sub input i (end_id - i) in
    tokenize_helper end_id (identify_keyword id_str :: acc)
  and read_operator i acc =
    match input.[i] with
    | '"' -> read_string (i + 1) acc
    | '+' -> tokenize_helper (i + 1) (PLUS :: acc)
    | '-' -> tokenize_helper (i + 1) (MINUS :: acc)
    | '*' -> tokenize_helper (i + 1) (TIMES :: acc)
    | '/' -> tokenize_helper (i + 1) (DIV :: acc)
    | '%' -> tokenize_helper (i + 1) (MOD :: acc)
    | '=' ->
        if i + 1 < length && input.[i + 1] = '=' then
          tokenize_helper (i + 2) (EQ :: acc)
        else
          tokenize_helper (i + 1) (ASSIGN :: acc)
    | '<' ->
        if i + 1 < length && input.[i + 1] = '=' then
          tokenize_helper (i + 2) (LEQ :: acc)
        else
          tokenize_helper (i + 1) (LT :: acc)
    | '>' ->
        if i + 1 < length && input.[i + 1] = '=' then
          tokenize_helper (i + 2) (GEQ :: acc)
        else
          tokenize_helper (i + 1) (GT :: acc)
    | '(' -> tokenize_helper (i + 1) (LPAREN :: acc)
    | ')' -> tokenize_helper (i + 1) (RPAREN :: acc)
    | ';' -> tokenize_helper (i + 1) (SEMI :: acc)
    | ',' -> tokenize_helper (i + 1) (COMMA :: acc)
    | '{' -> tokenize_helper (i + 1) (LBRACE :: acc)
    | '}' -> tokenize_helper (i + 1) (RBRACE :: acc)
    | '[' -> tokenize_helper (i + 1) (LBRACKET :: acc)
    | ']' -> tokenize_helper (i + 1) (RBRACKET :: acc)
    | _ -> raise (LexError ("Unexpected character: " ^ String.make 1 input.[i]))
  and read_string i acc =
    let rec collect_string j =
      if j < length && input.[j] <> '"' then collect_string (j + 1)
      else j
    in
    let end_string = collect_string i in
    if end_string >= length || input.[end_string] <> '"' then
      raise (LexError "Unterminated string literal")
    else
      let str = String.sub input i (end_string - i) in
      tokenize_helper (end_string + 1) (STRING str :: acc)
  and identify_keyword id =
    match id with
    | "let" -> LET
    | "print" -> PRINT
    | "if" -> IF
    | "else" -> ELSE
    | "and" -> AND
    | "or" -> OR
    | "not" -> NOT
    | "true" -> BOOL true
    | "false" -> BOOL false
    | "sort" -> SORT
    | "transform" -> TRANSFORM
    | "from" -> FROM
    | "to" -> TO
    | "WGS84" -> WGS84
    | "UTM" -> UTM

    (* TODO: Add support for additional keywords:
       - for
       - while
       - break
       - continue
       - in
       - func
       - return
       - shift_left
       - shift_right
       - swap
       - reverse
       - sort
       - filter
       - map
       - max
       - min
       - avg
       - median
       - as (for type casting)
       - type keywords (char, string, list)
    *)
    | _ -> ID id
  in
  tokenize_helper 0 []
