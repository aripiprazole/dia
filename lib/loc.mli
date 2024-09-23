type t = {
  file : string;
  start : int;
  ending : int;
}

val pp : Format.formatter -> t -> unit
val show : t -> string
val synthesized : t
