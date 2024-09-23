type kind =
  | K_prefix of string
  | K_infix of string
  | K_postfix of string
  | K_hole
[@@deriving show]

type t = S_symbol of kind * Loc.t [@@deriving show]

let name = function
| K_hole -> None
| K_postfix s
| K_infix s
| K_prefix s ->
    Some s

let map f (S_symbol (k, loc)) =
  match k with
  | K_hole -> S_symbol (K_hole, loc)
  | K_postfix s
  | K_infix s
  | K_prefix s ->
      S_symbol (f s, loc)

let make k = S_symbol (k, Loc.synthesized)
