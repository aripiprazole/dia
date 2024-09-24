type kind =
  | K_prefix
  | K_infix
  | K_postfix
  | K_hole
[@@deriving show]

type t =
  | S_symbol of {
      kind : kind;
      text : string;
      pos : Loc.t;
    }
[@@deriving show]

let text (S_symbol { text; _ }) = text
let make kind text = S_symbol { kind; text; pos = Loc.synthesized }
