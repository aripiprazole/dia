type parameter = {
  name : string Loc.t;
  icit : Core.icit;
}

type bound =
  | Bound
  | Defined

type meta_var = Meta_var of int

(* Core programming language for dia. It does have explicit
   substitutions for better error messages as [`Let`] *)
type t =
  | U
  | Src_pos of t Loc.t
  | Bvar of Debruijin.idx
  | Fvar of Debruijin.lvl
  | Lam of Symbol.t * Core.icit * t
  | App of t * (t * Core.icit) list
  | Hole of meta_var
  | Pi of dom * t
  | Subst of Symbol.t * t * t
  | Inserted_meta of meta_var * bound list

and dom =
  | Dom of {
      name : Symbol.t;
      icit : Core.icit;
      dom : t;
    }

let rec lam_dom ls = function
| Lam (dom, icit, cod) -> (dom, icit) :: lam_dom ls cod
| _ -> []

let rec lam_cod = function
| Lam (_, _, cod) -> lam_cod cod
| _ -> None
