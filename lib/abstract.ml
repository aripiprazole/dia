module Expr = struct
  (* Core language syntax *)
  type t =
    | E_u
    | E_src_pos of t Loc.t
    | E_var of string Loc.t
    | E_lam of Symbol.t * t
    | E_app of t * (Concrete.icit * t) list
    | E_as of t * t
    | E_hole of string Loc.t option
    | E_pi of Symbol.t * Concrete.icit * t * t
    | E_let of string Loc.t * t * t
  [@@deriving show]
end

module Top_level = struct
  type t =
    | T_let_decl of {
        name : string Loc.t;
        tt : Expr.t;
        value : Expr.t;
      }
  [@@deriving show]
end
