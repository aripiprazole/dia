module Expr : sig
  type t =
    | U
    | Src_pos of t Loc.t
    | Var of string Loc.t
    | Lam of Symbol.t * t
    | App of t * (Syntax.icit * t) list
    | As of t * t
    | Hole of string Loc.t option
    | Pi of Symbol.t * Syntax.icit * t * t
    | Let of string Loc.t * t * t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module TopLevel : sig
  type t =
    | Definition of {
        name : string Loc.t;
        tt : Expr.t;
        value : Expr.t;
      }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
