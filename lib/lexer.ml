open Parser

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

exception Eof

let token buf =
  match%sedlex buf with
  | number -> NUMBER (int_of_string (Sedlexing.Latin1.lexeme buf))
  | Plus (Chars "+-*/^=<>!&|~?%:") -> INFIX_ID (Sedlexing.Latin1.lexeme buf)
  | letter, Star ('A' .. 'Z' | 'a' .. 'z' | digit) ->
      ID (Sedlexing.Latin1.lexeme buf)
  | "type" -> TYPE
  | "let" -> LET
  | "match" -> MATCH
  | "with" -> WITH
  | "fun" -> FUN
  | "->" -> ARROW
  | "=>" -> DOUBLE_ARROW
  | '(' -> LEFT_PARENS
  | ')' -> RIGHT_PARENS
  | '{' -> LEFT_BRACES
  | '}' -> RIGHT_BRACES
  | eof -> raise Eof
  | _ -> failwith @@ "Unexpected character: " ^ Sedlexing.Latin1.lexeme buf
