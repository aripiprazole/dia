type eval_error =
  | EE_panic of string
  | EE_fail_with_pos of Loc.pos * eval_error

exception Eval_error of eval_error

let ( >> ) f g x = g (f x)
let ( <-- ) m v = Effect.perform @@ Value.Update_meta_var (m, v)
let fresh () = Effect.perform @@ Value.Fresh_meta_var ()
let lookup n = Effect.perform @@ Value.Lookup_meta_var n

let apply_term icit tt arg =
  match tt with
  | Term.App (callee, arguments) -> Term.App (callee, (arg, icit) :: arguments)
  | _ -> tt

(* Evaluates term into normal form *)
let rec eval env = function
  | Term.U -> Value.U
  | Term.Bvar { value; _ } -> List.nth env value
  | Term.Src_pos { value; _ } -> eval env value
  | Term.Pi (Term.Dom { name; dom; icit }, cod) ->
      let dom = eval env dom in
      Value.Pi (name, icit, dom, Closure { env; expr = cod })
  | Term.Lam (dom, icit, cod) ->
      Value.Lam (dom, icit, Closure { env; expr = cod })
  | Term.Subst (_, _, _) -> assert false
  | Term.Hole h -> begin
      match lookup h with
      | Value.Solved v -> v
      | Value.Unsolved -> Value.Flex (h, [])
    end
  | Term.App (callee, arguments) -> begin
      let pos = assert false in
      try apply_sp env callee arguments with
      | Eval_error ee -> raise (Eval_error (EE_fail_with_pos (pos, ee)))
    end
  | Term.Fvar _ -> raise @@ Eval_error (EE_panic "unimplemented")
  | Term.Inserted_meta _ -> raise @@ Eval_error (EE_panic "unimplemented")

(* Applies a spine of arguments to a function *)
and apply_sp env callee arguments =
  arguments
  |> List.map (fst >> eval env)
  |> List.fold_left ( $$ ) (eval env callee)

(* Applies a closure *)
and ( $$$ ) (Value.Closure { env; expr }) argument = eval (argument :: env) expr

(* Applies a closure *)
and ( $$ ) callee argument =
  match callee with
  | Value.Lam (_, _, Value.Closure { env; expr }) -> eval (argument :: env) expr
  | Value.Flex (m, sp) -> Value.Flex (m, (argument, Expl) :: sp)
  | Value.Rigid (m, sp) -> Value.Rigid (m, (argument, Expl) :: sp)
  | _ -> raise @@ Eval_error (EE_panic "impossible")

(* Takes out of flexible type values, forcing it into their original forms, so
   we can easily compare them. *)
let rec force = function
  | Value.Flex (m, sp) as v -> begin
      match lookup m with
      | Value.Unsolved -> v
      | Value.Solved callee ->
          force (sp |> List.map fst |> List.fold_left ( $$ ) callee)
    end
  | otherwise -> otherwise

(* Readback values into core expressions *)
let rec quote l value =
  match value |> force with
  | Value.Flex (m, sp) ->
      let arguments = sp |> List.map (fun (v, icit) -> (quote l v, icit)) in
      Term.App (Term.Hole m, arguments)
  | Value.Rigid (x, sp) ->
      let callee = Term.Bvar (Debruijin.lvl_to_idx l x) in
      let arguments = sp |> List.map (fun (v, icit) -> (quote l v, icit)) in
      Term.App (callee, arguments)
  | Value.Lam (domain, icit, _) as lam ->
      let codomain = lam $$ Value.Rigid (l, []) |> quote (Debruijin.shift l) in
      Term.Lam (domain, icit, codomain)
  | Value.Pi (name, icit, dom, cod) ->
      let dom = quote l dom in
      let cod = Value.Lam (name, icit, cod) in
      let cod = cod $$ Value.Rigid (l, []) |> quote (Debruijin.shift l) in
      Term.Pi (Dom { name; icit; dom }, cod)
  | Value.U -> Term.U

(* Normalises into normal form by: quoting and evaluating it after. *)
let nf env = quote (List.length env) >> eval env
