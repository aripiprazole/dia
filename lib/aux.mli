type eval_error =
  | EE_panic of string
  | EE_fail_with_pos of Loc.pos * eval_error

exception Eval_error of eval_error

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( <-- ) : Term.meta_var -> Value.t -> unit
