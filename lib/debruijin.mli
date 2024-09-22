type lvl = int

val pp_lvl : Format.formatter -> lvl -> unit
val show_lvl : lvl -> string

type idx =
  | Idx of {
      value : lvl;
      name : string Loc.t;
    }

val pp_idx : Format.formatter -> idx -> unit
val show_idx : idx -> string
val lvl_to_idx : int -> int -> idx
val shift : int -> int
