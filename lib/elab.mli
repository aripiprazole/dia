module Ren : sig
  type 'a t
end

type pren = { dom : int; cod : int; ren : int Ren.t }
type error_kind = E_unify_error | E_occurs_check | E_escaping_variable
type error = { ctx : Ctx.t; kind : error_kind }

exception Unification_error of error_kind
exception Error of error

val lift : pren -> pren
val invert : int -> (Value.t * 'a) list -> pren
val rename : Term.meta_var -> pren -> Value.t -> Term.t

val solve :
  int -> Term.meta_var -> (Value.t * Concrete.icit) list -> Value.t -> unit

val unify : int -> Value.t -> Value.t -> Value.t
val unify_catch : Ctx.t -> Value.t -> Value.t -> Value.t
val fresh_meta : Ctx.t -> Term.t
val insert : Ctx.t -> Term.t * Value.t -> Term.t * Value.t
val check : Ctx.t -> Abstract.Expr.t -> Value.t -> Term.t
val infer : Ctx.t -> Abstract.Expr.t -> Term.t * Value.t
