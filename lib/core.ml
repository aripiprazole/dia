type icit =
  | Expl
  | Impl

module Expr = struct
  (* Core language syntax *)
  type t =
    | U
    | Src_pos of t Loc.t
    | Var of string Loc.t
    | Lam of Symbol.t * t
    | App of t * (icit * t) list
    | As of t * t
    | Hole of string Loc.t option
    | Pi of Symbol.t * icit * t * t
    | Let of string Loc.t * t * t
    | Data of {
        name : string Loc.t;
        tt : t;
        constructors : constructor list;
        next : t;
      }

  and constructor = {
    name : string Loc.t;
    tt : t;
  }
end

module TopLevel = struct
  type t =
    | Inductive of {
        name : string Loc.t;
        tt : Expr.t;
        constructors : constructor list;
      }
    | Definition of {
        name : string Loc.t;
        tt : Expr.t;
        value : Expr.t;
      }

  and constructor = {
    name : string Loc.t;
    tt : Expr.t;
  }
end
