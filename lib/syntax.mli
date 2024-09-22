type icit =
  | Expl
  | Impl

val pp_icit : Format.formatter -> icit -> unit
val show_icit : icit -> string

type name =
  | Prefix of Symbol.t
  | Infix of Symbol.t
  | Postfix of Symbol.t

val pp_name : Format.formatter -> name -> unit
val show_name : name -> string

module Pattern : sig
  type t =
    | Constructor of {
        name : name;
        args : t list;
      }
    | Var of Symbol.t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Expr : sig
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

  and constructor =
    | Constructor of {
        name : name;
        tt : t;
      }

  and parameter =
    | Parameter of {
        names : Symbol.t list;
        icit : icit;
        tt : t;
      }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val pp_constructor : Format.formatter -> constructor -> unit
  val show_constructor : constructor -> string
  val pp_parameter : Format.formatter -> parameter -> unit
  val show_parameter : parameter -> string
end

module TopLevel : sig
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

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

type program =
  | Program of {
      hashbang : string Loc.t option;
      declarations : TopLevel.t list;
    }

val pp_program : Format.formatter -> program -> unit
val show_program : program -> string
val e_app : Expr.t -> icit -> Expr.t -> Expr.t
val e_let : Symbol.t -> Expr.t -> Expr.t -> Expr.t
val e_match : Expr.t -> (Pattern.t * Expr.t) list -> Expr.t
val e_lam : Symbol.t list -> Expr.t -> Expr.t
val e_pi : Expr.t -> Expr.t -> Expr.t
val curry : Expr.t -> (icit * Expr.t) list -> Expr.t
