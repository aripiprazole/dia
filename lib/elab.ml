open Aux
open Term
open Value
open Symbol
open Abstract.Expr
module Ren = Map.Make (Int)

(* Partial renaming *)
type pren = {
  dom : Debruijin.lvl; (* domain *)
  cod : Debruijin.lvl; (* codomain *)
  ren : Debruijin.lvl Ren.t; (* renaming map *)
}

type error_kind =
  | E_unify_error
  | E_occurs_check
  | E_escaping_variable

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
    | V_rigid (lvl, []) when Option.is_none (Ren.find_opt lvl ren) ->
        let dom = dom + 1 in
        let ren = ren |> Ren.add lvl dom in
        { dom; ren; cod = gamma }
    | _ -> raise @@ Unification_error E_unify_error
  end

(* Perform the partial renaming on rhs, while also checking for "m" occurrences. *)
let rec rename m pren v =
  let rec tt_app tt pren = function
  | [] -> tt
  | sp ->
      let sp' = sp |> List.map (fun (v, icit) -> (rename m pren v, icit)) in
      T_app (tt_app tt pren sp, sp')
  in

  match v |> force with
  | V_flex (m', sp) ->
      if m = m' then raise @@ Unification_error E_occurs_check
      else tt_app (T_hole m') pren sp
  | V_rigid (x, sp) -> begin
      match Ren.find_opt x pren.ren with
      | None -> raise @@ Unification_error E_escaping_variable
      | Some x' -> tt_app (T_bvar (Debruijin.lvl_to_idx pren.dom x')) pren sp
    end
  | V_lam (name, icit, closure) ->
      let cod = rename m (lift pren) (closure $$$ Value.var pren.cod) in
      T_lam (name, icit, cod)
  | V_pi (name, icit, dom, cod) ->
      let dom = Term.Dom { icit; name; dom = rename m pren dom } in
      let cod = rename m (lift pren) (cod $$$ Value.var pren.cod) in
      T_pi (dom, cod)
  | V_u -> T_u

let solve gamma h sp rhs =
  let partial_renaming = invert gamma sp in
  let rhs = rename h partial_renaming rhs in
  let lams =
    sp (* Transforms into a implicitness list *)
    |> List.map snd (* Reverts the list *)
    |> List.rev (* Builds lambdas over lambdas *)
    |> List.fold_left
         (fun acc next -> T_lam (Symbol.make K_hole, next, acc))
         rhs
  in
  h <-- eval [] lams

(* Unifies two types in a single one *)
let rec unify l t u =
  match (t |> force, u |> force) with
  | V_u, V_u -> V_u
  (* Pi unification *)
  | V_pi (_, i, dom, cod), V_pi (_, i', dom', cod') when i = i' ->
      let _ = unify l dom dom' in
      let _ =
        unify (Debruijin.shift l) (cod $$$ Value.var l) (cod' $$$ Value.var l)
      in
      t
  (* Lambda unification *)
  | V_lam (_, _, _), V_lam (_, _, _)
  | _, V_lam (_, _, _)
  | V_lam (_, _, _), _ ->
      unify (Debruijin.shift l) (t $$ Value.var l) (u $$ Value.var l)
  (* Unification of meta variables, it does unifies meta variables that
     are present in the context. *)
  | V_flex (m, sp), u
  | u, V_flex (m, sp) ->
      solve l m sp u;
      t
  | _ -> raise @@ Unification_error E_unify_error

let unify_catch (Ctx.{ lvl = l; _ } as ctx) t u =
  try unify l t u with
  | Unification_error kind -> raise @@ Error { ctx; kind }

let fresh_meta Ctx.{ bounds; _ } = T_inserted_meta (fresh (), bounds)

let rec insert ctx = function
| (T_lam (_, Concrete.Impl, _) as tt), va -> (tt, va)
| tt, va -> begin
    match va with
    | V_pi (_, Concrete.Impl, _, cod) ->
        let m = fresh_meta ctx in
        let mv = eval ctx.env m in
        insert ctx (apply_term Concrete.Impl tt m, cod $$$ mv)
    | _ -> (tt, va)
  end

let rec check ctx t expected =
  match (t, expected) with
  | E_src_pos (value, pos), type_repr ->
      check Ctx.{ ctx with pos } value type_repr
  | E_lam _, V_pi (_, _, _, _) -> assert false
  | E_hole _, _ -> fresh_meta ctx
  | t, expected ->
      let t, inferred = insert ctx (infer ctx t) in
      let _ = unify_catch ctx expected inferred in
      t

and infer ctx = function
| E_u -> (T_u, V_u)
| E_var _ -> assert false
| E_src_pos (value, pos) -> infer { ctx with pos } value
| E_lam _ -> assert false
| E_app _ -> assert false
| E_as (term, expected) -> begin
    let expected = eval ctx.env @@ check ctx expected V_u in
    let term = check ctx term expected in
    (term, expected)
  end
| E_hole _ ->
    let meta = fresh_meta ctx in
    (meta, eval ctx.env @@ fresh_meta ctx)
| E_pi (name, icit, dom, cod) ->
    let dom = check ctx dom V_u in
    let cod = check (ctx |> Ctx.bind name (eval ctx.env dom)) cod V_u in
    (T_pi (Dom { name; icit; dom }, cod), V_u)
| E_let (_, _, _) -> assert false
