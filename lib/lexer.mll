{
  open Parser
}

let var = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
    [' ' '\t' '\n' '\r']+ { token lexbuf }
  | "->" { ARROW }
  | "=>" { DOUBLE_ARROW }
  | "let" { LET }
  | "in" { IN }
  | "match" { MATCH }
  | "with" { WITH }
  | "fun" { FUN }
  | "|" { BAR }
  | "(" { LEFT_PARENS }
  | ")" { RIGHT_PARENS }
  | "{" { LEFT_BRACES }
  | "}" { RIGHT_BRACES }
  | "." { DOT }
  | "=" { EQUALS }
  | ":" { COLON }
  | "?" { QUESTION_MARK }
  | var { ID (Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }

