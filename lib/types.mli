open Loc
open Term

type eval_error =
  | EE_panic of string
  | EE_fail_with_pos of pos * eval_error

exception Eval_error of eval_error

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(* Evaluates term into normal form *)
val eval : value list -> term -> value

(* Applies a closure *)
val ( $$$ ) : closure -> value -> value

(* Applies a closure *)
val ( $$ ) : value -> value -> value

(* Shifts level up *)
val shift : lvl -> lvl

(* Inverts the levels into indexes *)
val lvl_to_idx : lvl -> lvl -> idx

(* Readback values into core expressions *)
val quote : lvl -> value -> term

(* Normalises into normal form by: quoting and evaluating it after. *)
val nf : value list -> value -> value

(* Variable *)
val v_var : lvl -> value

(* Flexible variable *)
val v_meta : hole ref -> value
