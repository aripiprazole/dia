type icit =
  | Expl
  | Impl

type name =
  | Prefix of string Loc.t
  | Infix of string Loc.t
  | Postfix of string Loc.t

module Pattern = struct
  type t =
    | Constructor of {
        name : name;
        args : t list;
      }
    | Var of Symbol.t
end

module Expr = struct
  (* Core language syntax *)
  type t =
    | U
    | Num of int
    | Src_pos of t Loc.t
    | Var of Symbol.t
    | Lam of {
        params : Symbol.t list;
        body : t;
      }
    | App of {
        callee : t;
        icit : icit;
        arg : t;
      }
    | As of {
        expr : t;
        tt : t;
      }
    | Hole of Symbol.t
    | Pi of {
        domain : parameter;
        codomain : t;
      }
    | Let of {
        name : Symbol.t;
        value : t;
        next : t;
      }
    | Match of {
        scrutinee : t;
        cases : (Pattern.t * t) list;
      }
    | Inductive of constructor list

  and constructor =
    | Constructor of {
        name : name;
        tt : t;
      }

  and parameter =
    | Parameter of {
        names : string Loc.t list;
        icit : icit;
        tt : t;
      }
end

module TopLevel = struct
  type t =
    | Src_pos of t Loc.t
    | Definition of {
        name : name;
        parameters : Expr.parameter list;
        tt : Expr.t;
        value : Expr.t;
      }
    | Pragma of {
        name : string Loc.t;
        value : string Loc.t;
      }
end

type program =
  | Program of {
      hashbang : string Loc.t option;
      declarations : TopLevel.t list;
    }

let e_app callee icit arg = Expr.App { callee; icit; arg }
let e_let name value next = Expr.Let { name; value; next }
let e_match scrutinee cases = Expr.Match { scrutinee; cases }
let e_lam params body = Expr.Lam { params; body }
let e_pi domain codomain = Expr.Pi { domain; codomain }
