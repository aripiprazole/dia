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

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with
    | End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let () =
  let _ = Parse_dia.parse_file (String.concat "\n" (read_lines !input_file)) in
  ()
