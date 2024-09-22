type origin =
  | Inserted
  | Source

type t = {
  pos : Loc.pos;
  env : Value.t list;
  lvl : int;
  bounds : Term.bound list;
  types : (Symbol.t * origin * Value.t) list;
}

val shift : t -> t
val bind : Symbol.t -> Value.t -> t -> t
