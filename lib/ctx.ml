type origin =
  | Inserted
  | Source

type t = {
  pos : Loc.pos; (* error handling *)
  env : Value.t List.t; (* nbe *)
  lvl : Debruijin.lvl; (* unification *)
  bounds : Term.bound list; (* fresh meta creating *)
  types : (string * origin * Value.t) list; (* pretty printing *)
}

let shift ({ lvl; _ } as ctx) = { ctx with lvl = Debruijin.shift lvl }
