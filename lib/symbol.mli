type kind =
  | K_prefix of string
  | K_infix of string
  | K_postfix of string
  | K_hole

type t = S_symbol of kind * Loc.t

val pp : Format.formatter -> t -> unit
val show : t -> string
val make : kind -> t
val name : kind -> string option
val map : (string -> kind) -> t -> t
