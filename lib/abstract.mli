module Expr : sig
  type t =
    | E_u
    | E_src_pos of t * Loc.t
    | E_var of Symbol.t
    | E_lam of Symbol.t * t
    | E_app of t * (Concrete.icit * t) list
    | E_as of t * t
    | E_hole of Symbol.t option
    | E_pi of dom * t
    | E_let of Symbol.t * t * t

  and dom = Dom of Symbol.t * Concrete.icit * t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Top_level : sig
  type t = T_let_decl of { name : Symbol.t; tt : Expr.t; value : Expr.t }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
