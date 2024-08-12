type icit =
  | Expl
  | Impl

(* Core language syntax *)
type t =
  | U
  | Src_pos of t Loc.t
  | Var of string Loc.t
  | Lam of Symbol.t * t
  | App of t * (icit * t) list
  | As of t * t
  | Hole of string Loc.t option
  | Pi of Symbol.t * icit * t * t
  | Let of string Loc.t * t * t
