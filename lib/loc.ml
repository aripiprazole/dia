open Lexing

type t =
  | Location of {
      startpos : position;
          [@printer
            fun fmt value ->
              fprintf fmt "%s:%d:%d" value.pos_fname value.pos_lnum
                value.pos_cnum]
      endpos : position;
          [@printer
            fun fmt value -> fprintf fmt "%d:%d" value.pos_lnum value.pos_cnum]
    }
  | Nowhere
[@@deriving show]

let make_location startpos endpos = Location { startpos; endpos }
