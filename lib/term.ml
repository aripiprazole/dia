open Loc

(* Optional name with debruijin index *)
type name = string loc option

(* Implicitness or explicitness of a lambda or a pi type *)
type icit =
  | Impl
  | Expl

type parameter = {
  name : string loc;
  icit : icit;
}

type bound =
  | Bound
  | Defined

type lvl = int

type idx = {
  value : int;
  name : string loc;
}

(* Programming language syntax *)
type re_expr =
  | RE_src_pos of re_expr loc
  | RE_var of string loc
  | RE_lam of Symbol.t * re_expr
  | RE_app of re_expr * re_spine
  | RE_hole of string loc option
  | RE_pi of name * re_expr * re_expr
  | RE_let of string loc * re_expr * re_expr
  | RE_cons of re_expr * re_expr

and re_spine = (icit * re_expr) list

type ctx = {
  lvl : lvl;
  pos : pos;
  bds : bound list;
  env : v_env;
  names : (string * value) list;
}

and value =
  | V_flex of hole ref * v_spine
  | V_rigid of lvl * v_spine
  | V_lam of name * icit * closure
  | V_pi of name * icit * value * closure
  | V_u

and closure =
  | Closure of {
      env : v_env;
      expr : term;
    }

and hole =
  | Unsolved
  | Solved of value

and v_spine = (value * icit) list
and v_env = value list

(* Core programming language for dia. It does have explicit
   substitutions for better error messages as [`Let`] *)
and term =
  | TT_u
  | TT_src_pos : term loc -> term
  | TT_bvar : idx -> term
  | TT_fvar : lvl -> term
  | TT_lam : name * icit * term -> term
  | TT_app : term * tt_spine -> term
  | TT_hole : hole ref -> term
  | TT_pi : tt_domain * term -> term
  | TT_subst : name * term * term -> term
  | TT_inserted_meta : value * bound list -> term

and tt_domain =
  | TTDomain of {
      name : name;
      icit : icit;
      domain : term;
    }

and tt_spine = (term * icit) list
