module I = Parser.MenhirInterpreter

exception Syntax_error of ((int * int) option * string)

val get_parse_error : 'a I.env -> string
val parse : Sedlexing.lexbuf -> 'a I.checkpoint -> 'a
val parse_file : string -> (Concrete.program, string) result
