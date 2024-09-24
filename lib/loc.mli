type t =
  | Location of { startpos : Lexing.position; endpos : Lexing.position }
  | Nowhere

val pp : Format.formatter -> t -> unit
val show : t -> string
val make_location : Lexing.position -> Lexing.position -> t
