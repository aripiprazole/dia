type t = string Loc.t option

val pp : Format.formatter -> t -> unit
val show : t -> string
val make : 'a -> 'a Loc.t option
