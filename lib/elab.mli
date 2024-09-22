module Ren : sig
  type key = int
  type 'a t = 'a Value.Meta_env.t
end

type pren = {
  dom : int;
  cod : int;
  ren : int Ren.t;
}

type error_kind =
  | Unify_error
  | Occurs_check
  | Escaping_variable

type error = {
  ctx : Ctx.t;
  kind : error_kind;
}

exception Unification_error of error_kind
exception Error of error

val lift : pren -> pren
val invert : int -> (Value.t * 'a) list -> pren
val rename : Term.meta_var -> pren -> Value.t -> Term.t

val solve :
  int -> Term.meta_var -> (Value.t * Syntax.icit) list -> Value.t -> unit

val unify : int -> Value.t -> Value.t -> Value.t
val unify_catch : Ctx.t -> Value.t -> Value.t -> Value.t
val fresh_meta : Ctx.t -> Term.t
val insert : Ctx.t -> Term.t * Value.t -> Term.t * Value.t
val check : Ctx.t -> Core.Expr.t -> Value.t -> Term.t
val infer : Ctx.t -> Core.Expr.t -> Term.t * Value.t
