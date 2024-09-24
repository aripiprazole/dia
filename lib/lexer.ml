open Parser
open Sedlexing.Utf8

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let rec token buf =
  match%sedlex buf with
  | "type" -> TYPE
  | "let" -> LET
  | "match" -> MATCH
  | "with" -> WITH
  | "fun" -> FUN
  | "#pragma" -> PRAGMA
  | "->" -> ARROW
  | "=>" -> DOUBLE_ARROW
  | "=" -> EQUALS
  | ":" -> COLON
  | "|" -> BAR
  | '(' -> LEFT_PARENS
  | ')' -> RIGHT_PARENS
  | '{' -> LEFT_BRACES
  | '}' -> RIGHT_BRACES
  | Plus (Chars " \n\t") -> token buf
  | number -> NUMBER (int_of_string (lexeme buf))
  | Plus (Chars "+-*/^=<>!&|~?%:") -> INFIX_ID (lexeme buf)
  | letter, Star (letter | digit | Chars "+-*/^=<>!&|~?%:") -> ID (lexeme buf)
  | eof -> EOF
  | _ -> failwith @@ "Unexpected character: " ^ Sedlexing.Latin1.lexeme buf
