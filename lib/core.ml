type icit =
  | Expl
  | Impl

(* Core language syntax *)
type t =
  | Src_pos of t Loc.t
  | Var of string Loc.t
  | Lam of Symbol.t * t
  | App of t * (icit * t) list
  | Hole of string Loc.t option
  | Pi of Symbol.t * t * t
  | Let of string Loc.t * t * t
