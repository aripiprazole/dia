open Dia

let help () =
  prerr_endline "Usage: dia [file]";
  exit 1

let verbose = ref false
let input_file = ref ""

let speclist =
  [
    ("-help", Arg.Unit help, "Display help");
    ("-verbose", Arg.Set verbose, "Display verbose output");
  ]

let usage = "Dia: a dependently typed lambda"
let () = Arg.parse speclist (fun filename -> input_file := filename) usage

let () =
  try
    let lexbuf = open_in !input_file |> Lexing.from_channel in
    let ip = Parser.Incremental.file lexbuf.lex_curr_p in
    let _ = Parse_dia.parse lexbuf ip in
    print_endline "Successfully parsed input"
  with
  | Parse_dia.Syntax_error (Some (line, column), msg) ->
      Printf.eprintf "Syntax error at line %d, column %d: %s\n" line column msg;
      exit 1
  | exn ->
      Printf.eprintf "An error occurred: %s\n" (Printexc.to_string exn);
      exit 1
