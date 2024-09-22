type pos = {
  file : string;
  start : int;
  ending : int;
}

val pp_pos : Format.formatter -> pos -> unit
val show_pos : pos -> string

type 'a t = {
  value : 'a;
  pos : pos;
}

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
val synthesized : pos
val unwrap : 'a t -> 'a
