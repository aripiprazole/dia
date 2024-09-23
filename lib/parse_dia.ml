open Lexing
open Sedlexing.Utf8
module I = Parser.MenhirInterpreter

exception Syntax_error of ((int * int) option * string)

let get_lexing_position lexbuf =
  let p, _ = Sedlexing.lexing_positions lexbuf in
  let line_number = p.pos_lnum in
  let column = p.pos_cnum - p.pos_bol + 1 in
  (line_number, column)

let get_parse_error env =
  match I.stack env with
  | (lazy Nil) -> "Invalid syntax"
  | (lazy (Cons (I.Element (state, _, _, _), _))) -> (
      try Parser_messages.message (I.number state) with
      | Not_found -> "invalid syntax (no specific message for this eror)")

let rec parse lexbuf checkpoint =
  match checkpoint with
  | I.InputNeeded _ ->
      let token = Lexer.token lexbuf in
      let startp, endp = Sedlexing.lexing_positions lexbuf in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError env ->
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error env in
      raise (Syntax_error (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
      raise (Syntax_error (None, "invalid syntax (parser rejected the input)"))

let parse_file text =
  try
    let lexbuf = from_string text in
    let startp, _ = Sedlexing.lexing_positions lexbuf in
    let ip = Parser.Incremental.file startp in
    Ok (parse lexbuf ip)
  with
  | Syntax_error (Some (line, column), msg) ->
      Printf.eprintf "Syntax error at line %d, column %d: %s\n" line column msg;
      Error msg
  | exn ->
      Printf.eprintf "An error occurred: %s\n" (Printexc.to_string exn);
      Error "Exception thrown"
