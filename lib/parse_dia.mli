module I = Parser.MenhirInterpreter

exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int
val get_parse_error : 'a I.env -> string
val parse : Lexing.lexbuf -> 'a I.checkpoint -> 'a
val parse_file : string -> (Syntax.program, string) result
