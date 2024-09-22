type icit =
  | Expl
  | Impl
[@@deriving show]

type name =
  | Prefix of Symbol.t
  | Infix of Symbol.t
  | Postfix of Symbol.t
[@@deriving show]

module Pattern = struct
  type t =
    | Constructor of {
        name : name;
        args : t list;
      }
    | Var of Symbol.t
  [@@deriving show]
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
    | Pi of {
        domain : t;
        codomain : t;
      }
    | Hole
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
  [@@deriving show]

  and constructor =
    | Constructor of {
        name : name;
        tt : t;
      }
  [@@deriving show]

  and parameter =
    | Parameter of {
        names : Symbol.t list;
        icit : icit;
        tt : t;
      }
  [@@deriving show]
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
        arguments : string Loc.t list;
      }
  [@@deriving show]
end

type program =
  | Program of {
      hashbang : string Loc.t option;
      declarations : TopLevel.t list;
    }
[@@deriving show]

let e_app callee icit arg = Expr.App { callee; icit; arg }
let e_let name value next = Expr.Let { name; value; next }
let e_match scrutinee cases = Expr.Match { scrutinee; cases }
let e_lam params body = Expr.Lam { params; body }
let e_pi domain codomain = Expr.Pi { domain; codomain }

let curry callee args =
  args
  |> List.fold_left
       (fun callee (icit, arg) -> Expr.App { callee; icit; arg })
       callee
