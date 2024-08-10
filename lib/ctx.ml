open Term

type origin =
  | Inserted
  | Source

type t = {
  pos : Loc.pos; (* error handling *)
  env : value list; (* nbe *)
  lvl : lvl; (* unification *)
  bds : bound list; (* fresh meta creating *)
  typ : (string * origin * value) list; (* pretty printing *)
}
