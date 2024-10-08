type t =
  | V_flex of Term.meta_var * spine
  | V_rigid of int * spine
  | V_lam of Symbol.t * Concrete.icit * closure
  | V_pi of Symbol.t * Concrete.icit * t * closure
  | V_u

and closure = Closure of { env : t list; expr : Term.t }
and spine = (t * Concrete.icit) list

val pp : Format.formatter -> t -> unit
val show : t -> string
val pp_closure : Format.formatter -> closure -> unit
val show_closure : closure -> string
val pp_spine : Format.formatter -> spine -> unit
val show_spine : spine -> string

type hole = Solved of t | Unsolved

val var : int -> t
val meta : Term.meta_var -> t

type _ Effect.t += Lookup_meta_var : Term.meta_var -> hole Effect.t
type _ Effect.t += Update_meta_var : Term.meta_var * t -> unit Effect.t
type _ Effect.t += Fresh_meta_var : unit -> Term.meta_var Effect.t

val ( <-- ) : Term.meta_var -> t -> unit
val fresh : unit -> Term.meta_var
val lookup : Term.meta_var -> hole

module Meta_env : sig
  type key = int
  type 'a t = 'a Map.Make(Int).t
end

val try_with :
  (unit -> 'a Effect.Deep.effect_handler -> 'b) ->
  'b Effect.Deep.effect_handler ->
  'b
