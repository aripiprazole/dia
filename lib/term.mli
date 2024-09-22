type parameter = {
  name : string Loc.t;
  icit : Syntax.icit;
}

val pp_parameter : Format.formatter -> parameter -> unit
val show_parameter : parameter -> string

type bound =
  | Bound
  | Defined

val pp_bound : Format.formatter -> bound -> unit
val show_bound : bound -> string

type meta_var = Meta_var of int

val pp_meta_var : Format.formatter -> meta_var -> unit
val show_meta_var : meta_var -> string

type t =
  | U
  | Src_pos of t Loc.t
  | Bvar of Debruijin.idx
  | Fvar of int
  | Lam of Symbol.t * Syntax.icit * t
  | App of t * (t * Syntax.icit) list
  | Hole of meta_var
  | Pi of dom * t
  | Subst of Symbol.t * t * t
  | Inserted_meta of meta_var * bound list

and dom =
  | Dom of {
      name : Symbol.t;
      icit : Syntax.icit;
      dom : t;
    }

val pp : Format.formatter -> t -> unit
val pp_dom : Format.formatter -> dom -> unit
val show : t -> string
val show_dom : dom -> string
val pi_dom : t -> dom list
val pi_cod : t -> 'a option
val lam_dom : t -> (Symbol.t * Syntax.icit) list
val lam_cod : t -> 'a option
