open Types
module Ren = Map.Make (Int)

(* Partial renaming *)
type pren = {
  dom : Debruijin.lvl; (* domain *)
  cod : Debruijin.lvl; (* codomain *)
  ren : Debruijin.lvl Ren.t; (* renaming map *)
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
let lift { dom; cod; ren } =
  let ren = Ren.add cod dom ren in
  { dom = dom + 1; cod = cod + 1; ren }

(* Γ : Debruijin.lvl, Δ : Value.spine
   -------------------------------------------
   Partial_renaming Γ Δ *)
let rec invert gamma = function
| [] -> { dom = 0; cod = gamma; ren = Ren.empty }
| (t, _) :: sp -> begin
    let { dom; ren; _ } = invert gamma sp in
    match t |> force with
    | Value.Rigid (lvl, []) when Option.is_none (Ren.find_opt lvl ren) ->
        let dom = dom + 1 in
        let ren = ren |> Ren.add lvl dom in
        { dom; ren; cod = gamma }
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
      match Ren.find_opt x pren.ren with
      | None -> raise @@ Unification_error Escaping_variable
      | Some x' -> tt_app (Term.Bvar (Debruijin.lvl_to_idx pren.dom x')) pren sp
    end
  | Value.Lam (name, icit, closure) ->
      let cod = rename m (lift pren) (closure $$$ Value.var pren.cod) in
      Term.Lam (name, icit, cod)
  | Value.Pi (name, icit, dom, cod) ->
      let dom = Term.Dom { icit; name; dom = rename m pren dom } in
      let cod = rename m (lift pren) (cod $$$ Value.var pren.cod) in
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

let rec insert ctx = function
| (Term.Lam (_, Core.Impl, _) as tt), va -> (tt, va)
| tt, va -> begin
    match va with
    | Value.Pi (_, Core.Impl, _, cod) ->
        let m = fresh_meta ctx in
        let mv = eval ctx.env m in
        insert ctx (apply_term Core.Impl tt m, cod $$$ mv)
    | _ -> (tt, va)
  end

let rec check ctx t expected =
  match (t, expected) with
  | Core.Src_pos { pos; value }, type_repr ->
      check Ctx.{ ctx with pos } value type_repr
  | Core.Lam (_, _), Value.Pi (_, _, _, _) -> assert false
  | Core.Hole _, _ -> fresh_meta ctx
  | t, expected ->
      let t, inferred = insert ctx (infer ctx t) in
      let _ = unify_catch ctx expected inferred in
      t

and infer ctx = function
| Core.U -> (Term.U, Value.U)
| Core.Var _ -> assert false
| Core.Src_pos { pos; value } -> infer { ctx with pos } value
| Core.Lam (_, _) -> assert false
| Core.App (_, _) -> assert false
| Core.As (term, expected) -> begin
    let expected = eval ctx.env @@ check ctx expected Value.U in
    let term = check ctx term expected in
    (term, expected)
  end
| Core.Hole _ ->
    let meta = fresh_meta ctx in
    (meta, eval ctx.env @@ fresh_meta ctx)
| Core.Pi (name, icit, dom, cod) ->
    let dom = check ctx dom Value.U in
    let cod = check (ctx |> Ctx.bind name (eval ctx.env dom)) cod Value.U in
    (Term.Pi (Term.Dom { name; icit; dom }, cod), Value.U)
| Core.Let (_, _, _) -> assert false
