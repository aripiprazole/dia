open Term
open Value

type eval_error =
  | EE_panic of string
  | EE_fail_with_pos of Loc.pos * eval_error

exception Eval_error of eval_error

let ( >> ) f g x = g (f x)
let ( <-- ) m v = Effect.perform @@ Update_meta_var (m, v)
let fresh () = Effect.perform @@ Fresh_meta_var ()
let lookup n = Effect.perform @@ Lookup_meta_var n

let apply_term icit tt arg =
  match tt with
  | T_app (callee, arguments) -> T_app (callee, (arg, icit) :: arguments)
  | _ -> tt

(* Evaluates term into normal form *)
let rec eval env = function
| T_u -> V_u
| T_bvar (Debruijin.Idx { value; _ }) -> List.nth env value
| T_src_pos { value; _ } -> eval env value
| T_pi (Term.Dom { name; dom; icit }, cod) ->
    let dom = eval env dom in
    V_pi (name, icit, dom, Closure { env; expr = cod })
| T_lam (dom, icit, cod) -> V_lam (dom, icit, Closure { env; expr = cod })
| T_subst (_, _, _) -> assert false
| T_hole h -> begin
    match lookup h with
    | Solved v -> v
    | Unsolved -> V_flex (h, [])
  end
| T_app (callee, arguments) -> begin
    let pos = assert false in
    try apply_sp env callee arguments with
    | Eval_error ee -> raise (Eval_error (EE_fail_with_pos (pos, ee)))
  end
| T_fvar _ -> raise @@ Eval_error (EE_panic "unimplemented")
| T_inserted_meta _ -> raise @@ Eval_error (EE_panic "unimplemented")

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
  | _ -> raise @@ Eval_error (EE_panic "impossible")

(* Takes out of flexible type values, forcing it into their original forms, so
   we can easily compare them. *)
let rec force = function
| V_flex (m, sp) as v -> begin
    match lookup m with
    | Unsolved -> v
    | Solved callee -> force (sp |> List.map fst |> List.fold_left ( $$ ) callee)
  end
| otherwise -> otherwise

(* Readback values into core expressions *)
let rec quote l value =
  match value |> force with
  | V_flex (m, sp) ->
      let arguments = sp |> List.map (fun (v, icit) -> (quote l v, icit)) in
      T_app (T_hole m, arguments)
  | V_rigid (x, sp) ->
      let callee = T_bvar (Debruijin.lvl_to_idx l x) in
      let arguments = sp |> List.map (fun (v, icit) -> (quote l v, icit)) in
      T_app (callee, arguments)
  | V_lam (domain, icit, _) as lam ->
      let codomain = lam $$ V_rigid (l, []) |> quote (Debruijin.shift l) in
      T_lam (domain, icit, codomain)
  | V_pi (name, icit, dom, cod) ->
      let dom = quote l dom in
      let cod = V_lam (name, icit, cod) in
      let cod = cod $$ V_rigid (l, []) |> quote (Debruijin.shift l) in
      T_pi (Dom { name; icit; dom }, cod)
  | V_u -> T_u

(* Normalises into normal form by: quoting and evaluating it after. *)
let nf env = quote (List.length env) >> eval env
