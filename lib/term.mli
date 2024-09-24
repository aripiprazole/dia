type parameter = { name : Symbol.t; icit : Concrete.icit }

val pp_parameter : Format.formatter -> parameter -> unit
val show_parameter : parameter -> string

type bound = Bound | Defined

val pp_bound : Format.formatter -> bound -> unit
val show_bound : bound -> string

type meta_var = Meta_var of int

val pp_meta_var : Format.formatter -> meta_var -> unit
val show_meta_var : meta_var -> string

type t =
  | T_u
  | T_src_pos of t * Loc.t
  | T_bvar of Debruijin.idx
  | T_fvar of int
  | T_lam of Symbol.t * Concrete.icit * t
  | T_app of t * (t * Concrete.icit) list
  | T_hole of meta_var
  | T_pi of dom * t
  | T_subst of Symbol.t * t * t
  | T_inserted_meta of meta_var * bound list

and dom = Dom of { name : Symbol.t; icit : Concrete.icit; dom : t }

val pp : Format.formatter -> t -> unit
val pp_dom : Format.formatter -> dom -> unit
val show : t -> string
val show_dom : dom -> string
val pi_dom : t -> dom list
val pi_cod : t -> 'a option
val lam_dom : t -> (Symbol.t * Concrete.icit) list
val lam_cod : t -> 'a option
