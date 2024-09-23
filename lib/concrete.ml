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
    | P_constructor of {
        name : name;
        args : t list;
      }
    | P_var of Symbol.t
  [@@deriving show]
end

module Expr = struct
  (* Core language syntax *)
  type t =
    | E_u
    | E_hole
    | E_num of int
    | E_src_pos of t Loc.t
    | E_var of Symbol.t
    | E_lam of {
        params : Symbol.t list;
        body : t;
      }
    | E_app of {
        callee : t;
        icit : icit;
        arg : t;
      }
    | E_pi of {
        domain : t;
        codomain : t;
      }
    | E_let of {
        name : Symbol.t;
        value : t;
        next : t;
      }
    | E_match of {
        scrutinee : t;
        cases : (Pattern.t * t) list;
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

module Top_level = struct
  type t =
    | T_src_pos of t Loc.t
    | T_let_decl of {
        name : name;
        parameters : Expr.parameter list;
        tt : Expr.t;
        value : Expr.t;
      }
    | T_type_decl of {
        name : name;
        parameters : Expr.parameter list;
        tt : Expr.t;
        constructors : constructor list;
      }
    | T_pragma of {
        name : string Loc.t;
        arguments : string Loc.t list;
      }
  [@@deriving show]

  and constructor =
    | Constructor of {
        name : name;
        tt : t;
      }
  [@@deriving show]
end

type program =
  | Program of {
      hashbang : string Loc.t option;
      declarations : Top_level.t list;
    }
[@@deriving show]

let e_app callee icit arg = Expr.E_app { callee; icit; arg }
let e_let name value next = Expr.E_let { name; value; next }
let e_match scrutinee cases = Expr.E_match { scrutinee; cases }
let e_lam params body = Expr.E_lam { params; body }
let e_pi domain codomain = Expr.E_pi { domain; codomain }

let curry callee args =
  args
  |> List.fold_left
       (fun callee (icit, arg) -> Expr.E_app { callee; icit; arg })
       callee
