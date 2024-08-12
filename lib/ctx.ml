type origin =
  | Inserted
  | Source

type t = {
  pos : Loc.pos; (* error handling *)
  env : Value.t List.t; (* nbe *)
  lvl : Debruijin.lvl; (* unification *)
  bounds : Term.bound list; (* fresh meta creating *)
  types : (Symbol.t * origin * Value.t) list; (* pretty printing *)
}

let shift ({ lvl; _ } as ctx) = { ctx with lvl = Debruijin.shift lvl }

(* Extend context with a bound variable *)
let bind name type_repr { env; lvl; bounds; types; pos } =
  {
    env = Value.var lvl :: env;
    lvl = Debruijin.shift lvl;
    types = (name, Source, type_repr) :: types;
    bounds = Term.Bound :: bounds;
    pos;
  }
