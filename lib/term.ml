type parameter = {
  name : string Loc.t;
  icit : Concrete.icit;
}
[@@deriving show]

type bound =
  | Bound
  | Defined
[@@deriving show]

type meta_var = Meta_var of int [@@deriving show]

(* Core programming language for dia. It does have explicit
   substitutions for better error messages as [`Let`] *)
type t =
  | T_u
  | T_src_pos of t Loc.t
  | T_bvar of Debruijin.idx
  | T_fvar of Debruijin.lvl
  | T_lam of Symbol.t * Concrete.icit * t
  | T_app of t * (t * Concrete.icit) list
  | T_hole of meta_var
  | T_pi of dom * t
  | T_subst of Symbol.t * t * t
  | T_inserted_meta of meta_var * bound list
[@@deriving show]

and dom =
  | Dom of {
      name : Symbol.t;
      icit : Concrete.icit;
      dom : t;
    }
[@@deriving show]

let rec pi_dom = function
| T_pi (dom, cod) -> dom :: pi_dom cod
| _ -> []

let rec pi_cod = function
| T_pi (_, cod) -> pi_cod cod
| _ -> None

let rec lam_dom = function
| T_lam (dom, icit, cod) -> (dom, icit) :: lam_dom cod
| _ -> []

let rec lam_cod = function
| T_lam (_, _, cod) -> lam_cod cod
| _ -> None
