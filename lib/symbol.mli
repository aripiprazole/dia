type kind = K_prefix | K_infix | K_postfix | K_hole
type t = S_symbol of { kind : kind; text : string; pos : Loc.t }

val pp : Format.formatter -> t -> unit
val show : t -> string
val text : t -> string
val make : kind -> string -> t
