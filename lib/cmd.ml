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

let handle_input filename = input_file := filename

let main () =
  Arg.parse speclist handle_input "Dia: a dependently typed lambda";
  let _ =
    open_in !input_file |> Lexing.from_channel |> Parser.file Lexer.token
  in
  print_endline "Successfully parsed input"
