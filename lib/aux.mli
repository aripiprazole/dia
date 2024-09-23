type eval_error =
  | EE_panic of string
  | EE_fail_with_pos of Loc.pos * eval_error

exception Eval_error of eval_error

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( <-- ) : Term.meta_var -> Value.t -> unit
val ( $$$ ) : Value.closure -> Value.t -> Value.t
val ( $$ ) : Value.t -> Value.t -> Value.t
val fresh : unit -> Term.meta_var
val lookup : Term.meta_var -> Value.hole
val apply_term : Concrete.icit -> Term.t -> Term.t -> Term.t
val eval : Value.t list -> Term.t -> Value.t

val apply_sp :
  Value.t list -> Term.t -> (Term.t * Concrete.icit) list -> Value.t

val force : Value.t -> Value.t
val quote : int -> Value.t -> Term.t
val nf : Value.t list -> Value.t -> Value.t
