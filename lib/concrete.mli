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
    | P_constructor of {
        name : name;
        args : t list;
      }
    | P_var of Symbol.t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Expr : sig
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

  and parameter =
    | Parameter of {
        names : Symbol.t list;
        icit : icit;
        tt : t;
      }

  val pp : Format.formatter -> t -> unit
  val pp_parameter : Format.formatter -> parameter -> unit
  val show : t -> string
  val show_parameter : parameter -> string
end

module Top_level : sig
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

  and constructor =
    | Constructor of {
        name : name;
        tt : Expr.t;
      }

  val pp : Format.formatter -> t -> unit
  val pp_constructor : Format.formatter -> constructor -> unit
  val show : t -> string
  val show_constructor : constructor -> string
end

type program =
  | Program of {
      hashbang : string Loc.t option;
      declarations : Top_level.t list;
    }

val e_app : Expr.t -> icit -> Expr.t -> Expr.t
val e_let : Symbol.t -> Expr.t -> Expr.t -> Expr.t
val e_match : Expr.t -> (Pattern.t * Expr.t) list -> Expr.t
val e_lam : Symbol.t list -> Expr.t -> Expr.t
val e_pi : Expr.t -> Expr.t -> Expr.t
val curry : Expr.t -> (icit * Expr.t) list -> Expr.t
val pp_program : Format.formatter -> program -> unit
val show_program : program -> string
