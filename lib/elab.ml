open Types
module Re = Map.Make (Int)

type pren = {
  domain : Debruijin.lvl;
  codomain : Debruijin.lvl;
  rename : Debruijin.lvl Re.t;
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

(* Lifts over a new bound variable as the following rule presents:

   σ : Partial_renaming Γ Δ
   --------------------------------------------------
   lift σ : Partial_renaming (Γ, x : A[σ]) (Δ, x : A) *)
let lift { domain; codomain; rename } =
  let rename = Re.add codomain domain rename in
  { domain = domain + 1; codomain = codomain + 1; rename }

(* Γ : Debruijin.lvl, Δ : Value.spine
   -------------------------------------------
   Partial_renaming Γ Δ *)
let rec invert gamma = function
  | [] -> { domain = 0; codomain = gamma; rename = Re.empty }
  | (t, _) :: sp -> begin
      let { domain; rename; _ } = invert gamma sp in
      match t |> force with
      | Value.Rigid (lvl, []) when Option.is_none (Re.find_opt lvl rename) ->
          let domain = domain + 1 in
          let rename = rename |> Re.add lvl domain in
          { domain; rename; codomain = gamma }
      | _ -> raise @@ Unification_error Unify_error
    end

(* Perform the partial renaming on rhs, while also checking for "m" occurrences. *)
let rec rename m pren v =
  let rec tt_app tt pren sp =
    if List.is_empty sp then tt
    else
      let sp' = sp |> List.map (fun (v, icit) -> (rename m pren v, icit)) in
      Term.App (tt_app tt pren sp, sp')
  in

  match v |> force with
  | Value.Flex (m', sp) ->
      if m = m' then raise @@ Unification_error Occurs_check
      else tt_app (Term.Hole m') pren sp
  | Value.Rigid (x, sp) -> begin
      match Re.find_opt x pren.rename with
      | None -> raise @@ raise @@ Unification_error Escaping_variable
      | Some x' ->
          tt_app (Term.Bvar (Debruijin.lvl_to_idx pren.domain x')) pren sp
    end
  | Value.Lam (name, icit, closure) ->
      let cod = rename m (lift pren) (closure $$$ Value.var pren.codomain) in
      Term.Lam (name, icit, cod)
  | Value.Pi (name, icit, dom, cod) ->
      let dom = Term.Dom { icit; name; domain = rename m pren dom } in
      let cod = rename m (lift pren) (cod $$$ Value.var pren.codomain) in
      Term.Pi (dom, cod)
  | Value.U -> Term.U

let solve gamma h sp rhs =
  let partial_renaming = invert gamma sp in
  let rhs = rename h partial_renaming rhs in
  let lams =
    sp (* Transforms into a implicitness list *)
    |> List.map snd (* Reverts the list *)
    |> List.rev (* Builds lambdas over lambdas *)
    |> List.fold_left (fun acc next -> Term.Lam (None, next, acc)) rhs
  in
  h <-- eval [] lams

(* Unifies two types in a single one *)
let rec unify l t u =
  match (t |> force, u |> force) with
  | Value.U, Value.U -> Value.U
  (* Pi unification *)
  | Value.Pi (_, i, dom, cod), Value.Pi (_, i', dom', cod') when i = i' ->
      let _ = unify l dom dom' in
      let _ =
        unify (Debruijin.shift l) (cod $$$ Value.var l) (cod' $$$ Value.var l)
      in
      t
  (* Lambda unification *)
  | Value.Lam (_, _, _), Value.Lam (_, _, _)
  | _, Value.Lam (_, _, _)
  | Value.Lam (_, _, _), _ ->
      unify (Debruijin.shift l) (t $$ Value.var l) (u $$ Value.var l)
  (* Unification of meta variables, it does unifies meta variables that
     are present in the context. *)
  | Value.Flex (m, sp), u
  | u, Value.Flex (m, sp) ->
      solve l m sp u;
      t
  | _ -> raise @@ Unification_error Unify_error

let unify_catch (Ctx.{ lvl = l; _ } as ctx) t u =
  try unify l t u with
  | Unification_error kind -> raise @@ Error { ctx; kind }

let fresh_meta Ctx.{ bounds; _ } = Term.Inserted_meta (fresh (), bounds)

let insert ctx = function
  | (Term.Lam (_, Core.Impl, _) as tt), va -> (tt, va)
  | t, va -> assert false

let rec check ctx = function
  | Core.Src_pos { pos; value }, type_repr ->
      check Ctx.{ ctx with pos } (value, type_repr)
  | Core.Lam (_, _), Value.Pi (_, _, _, _) -> assert false
  | t, expected ->
      let t, inferred = insert ctx (infer ctx t) in
      let _ = unify_catch ctx expected inferred in
      t

and infer ctx = function
  | Core.Var _ -> assert false
  | Core.Src_pos { pos; value } -> infer { ctx with pos } value
  | Core.Lam (_, _) -> assert false
  | Core.App (_, _) -> assert false
  | Core.Hole _ -> assert false
  | Core.Pi (_, _, _) -> assert false
  | Core.Let (_, _, _) -> assert false
