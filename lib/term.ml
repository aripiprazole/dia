type parameter = {
  name : string Loc.t;
  icit : Syntax.icit;
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
  | U
  | Src_pos of t Loc.t
  | Bvar of Debruijin.idx
  | Fvar of Debruijin.lvl
  | Lam of Symbol.t * Syntax.icit * t
  | App of t * (t * Syntax.icit) list
  | Hole of meta_var
  | Pi of dom * t
  | Subst of Symbol.t * t * t
  | Inserted_meta of meta_var * bound list
[@@deriving show]

and dom =
  | Dom of {
      name : Symbol.t;
      icit : Syntax.icit;
      dom : t;
    }
[@@deriving show]

let rec pi_dom = function
| Pi (dom, cod) -> dom :: pi_dom cod
| _ -> []

let rec pi_cod = function
| Pi (_, cod) -> pi_cod cod
| _ -> None

let rec lam_dom = function
| Lam (dom, icit, cod) -> (dom, icit) :: lam_dom cod
| _ -> []

let rec lam_cod = function
| Lam (_, _, cod) -> lam_cod cod
| _ -> None
