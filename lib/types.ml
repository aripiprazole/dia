open Loc
open Term

let ( >> ) f g x = g (f x)

type eval_error =
  | EE_panic of string
  | EE_fail_with_pos of pos * eval_error

exception Eval_error of eval_error

type eval_ctx = { pos : pos }

(* Evaluates term into normal form *)
let rec eval env = eval' env { pos = Loc.synthesized }

(* Eval internal impl *)
and eval' env ({ pos } as ctx) = function
  | TT_u -> V_u
  | TT_bvar { value; _ } -> List.nth env value
  | TT_src_pos { value; pos } -> eval' env { pos } value
  | TT_pi (TTDomain { name; domain; icit }, codomain) ->
      let domain = eval' env ctx domain in
      V_pi (name, icit, domain, Closure { env; expr = codomain })
  | TT_lam (domain, icit, codomain) ->
      V_lam (domain, icit, Closure { env; expr = codomain })
  | TT_subst (_, value, next) -> eval' (eval' env ctx value :: env) ctx next
  | TT_hole ({ contents = Unsolved } as h) -> V_flex (h, [])
  | TT_hole { contents = Solved v } -> v
  | TT_app (callee, arguments) -> begin
      match apply_sp env callee arguments with
      | value -> value
      | exception Eval_error ee ->
          raise (Eval_error (EE_fail_with_pos (pos, ee)))
    end
  | TT_fvar _ -> raise (Eval_error (EE_panic "unimplemented"))
  | TT_inserted_meta _ -> raise (Eval_error (EE_panic "unimplemented"))

(* Applies a spine of arguments to a function *)
and apply_sp env callee arguments =
  arguments
  |> List.map (fst >> eval env)
  |> List.fold_left ( $$ ) (eval env callee)

(* Applies a closure *)
and ( $$$ ) (Closure { env; expr }) argument = eval (argument :: env) expr

(* Applies a closure *)
and ( $$ ) callee argument =
  match callee with
  | V_lam (_, _, Closure { env; expr }) -> eval (argument :: env) expr
  | V_flex (m, sp) -> V_flex (m, (argument, Expl) :: sp)
  | V_rigid (m, sp) -> V_rigid (m, (argument, Expl) :: sp)
  | _ -> raise (Eval_error (EE_panic "impossible"))

(* Takes out of flexible type values, forcing it into their original forms, so
   we can easily compare them. *)
let rec force = function
  | V_flex (m, sp) as v -> begin
      match !m with
      | Unsolved -> v
      | Solved callee ->
          force (sp |> List.map fst |> List.fold_left ( $$ ) callee)
    end
  | otherwise -> otherwise

(* Shifts level up *)
let shift l = l + 1

(* Inverts the levels into indexes *)
let lvl_to_idx l x =
  { name = { pos = Loc.synthesized; value = "_" }; value = l - x - 1 }

(* Readback values into core expressions *)
let rec quote l value =
  match force value with
  | V_flex (m, sp) ->
      let arguments = sp |> List.map (fun (v, icit) -> (quote l v, icit)) in
      TT_app (TT_hole m, arguments)
  | V_rigid (x, sp) ->
      let callee = TT_bvar (lvl_to_idx l x) in
      let arguments = sp |> List.map (fun (v, icit) -> (quote l v, icit)) in
      TT_app (callee, arguments)
  | V_lam (domain, icit, _) as lam ->
      let codomain = lam $$ V_rigid (l, []) |> quote (shift l) in
      TT_lam (domain, icit, codomain)
  | V_pi (name, icit, domain, codomain) ->
      let domain = quote l domain in
      let codomain = V_lam (name, icit, codomain) in
      let codomain = codomain $$ V_rigid (l, []) |> quote (shift l) in
      TT_pi (TTDomain { name; icit; domain }, codomain)
  | V_u -> TT_u

(* Normalises into normal form by: quoting and evaluating it after. *)
let nf env = quote (List.length env) >> eval env

(* Variable *)
let v_var x = V_rigid (x, [])

(* Flexible variable *)
let v_meta x = V_flex (x, [])
