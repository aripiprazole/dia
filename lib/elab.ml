open Term
open Types

let rec check ctx = function
  | RE_src_pos { pos; value }, type_repr -> check { pos } (value, type_repr)
  | RE_lam (parameter, value), V_pi (_, _, domain, codomain) -> assert false
  | t, expected -> assert false

let rec infer ctx = function
  | RE_var _ -> assert false
  | RE_src_pos { pos; value } -> infer { pos } value
  | RE_lam (parameters, expr) -> assert false
  | RE_app (callee, arguments) -> assert false
  | RE_hole _ -> assert false
  | RE_pi (_, _, _) -> assert false
  | RE_let (name, n, m) -> assert false
  | RE_cons (head, tail) -> infer ctx (RE_let (Symbol.make "_", head, tail))

exception Unification_error
exception Occurs_check
exception Escaping_variable

module Re = Map.Make (Int)

type pren = {
  domain : lvl;
  codomain : lvl;
  rename : lvl Re.t;
}

(* Lifts over a new bound variable as the following rule presents:

   σ : Partial_renaming Γ Δ
   --------------------------------------------------
   lift σ : Partial_renaming (Γ, x : A[σ]) (Δ, x : A) *)
let lift { domain; codomain; rename } =
  let rename = Re.add codomain domain rename in
  { domain = domain + 1; codomain = codomain + 1; rename }

(* Γ : ctx, Δ : spine
   -------------------------------------------
   Γ, (sp : Sub Δ Γ) ⊢ Partial_renaming Γ Δ *)
let rec invert gamma = function
  | [] -> { domain = 0; codomain = gamma; rename = Re.empty }
  | (t, _) :: sp -> begin
      let { domain; rename; codomain = _ } = invert gamma sp in
      match force t with
      | V_rigid (lvl, []) when Option.is_none (Re.find_opt lvl rename) ->
          let domain = domain + 1 in
          let rename = rename |> Re.add lvl domain in
          { domain; rename; codomain = gamma }
      | _ -> raise Unification_error
    end

(* Perform the partial renaming on rhs, while also checking for "m" occurrences. *)
let rec rename m pren v =
  let rec tt_app tt pren sp =
    if List.is_empty sp then tt
    else
      let sp' = sp |> List.map (fun (v, icit) -> (rename m pren v, icit)) in
      TT_app (tt_app tt pren sp, sp')
  in

  match force v with
  | V_flex (m', sp) ->
      if m = m' then raise Occurs_check else tt_app (TT_hole m') pren sp
  | V_rigid (x, sp) -> begin
      match Re.find_opt x pren.rename with
      | None -> raise Escaping_variable
      | Some x' -> tt_app (TT_bvar (lvl_to_idx pren.domain x')) pren sp
    end
  | V_lam (name, icit, closure) ->
      TT_lam (name, icit, rename m (lift pren) (closure $$$ v_var pren.codomain))
  | V_pi (name, icit, domain, codomain) ->
      let domain = TTDomain { icit; name; domain = rename m pren domain } in
      TT_pi (domain, rename m (lift pren) (codomain $$$ v_var pren.codomain))
  | V_u -> TT_u

let solve gamma h sp rhs =
  let partial_renaming = invert gamma sp in
  let rhs = rename h partial_renaming rhs in
  let lams =
    sp (* Transforms into a implicitness list *)
    |> List.map snd (* Reverts the list *)
    |> List.rev (* Builds lambdas over lambdas *)
    |> List.fold_left (fun acc next -> TT_lam (None, next, acc)) rhs
  in
  h := Solved (eval [] lams)

(* Unifies two types in a single one *)
let rec unify l t u =
  match (force t, force u) with
  | V_u, V_u -> V_u
  (* Pi unification *)
  | V_pi (_, i, dom, cod), V_pi (_, i', dom', cod') when i = i' ->
      let _ = unify l dom dom' in
      let _ = unify (shift l) (cod $$$ v_var l) (cod' $$$ v_var l) in
      t
  (* Lambda unification *)
  | V_lam (_, _, _), V_lam (_, _, _)
  | _, V_lam (_, _, _)
  | V_lam (_, _, _), _ ->
      unify (shift l) (t $$ v_var l) (u $$ v_var l)
  (* Unification of meta variables, it does unifies meta variables that
     are present in the context. *)
  | V_flex (m, sp), u
  | u, V_flex (m, sp) ->
      solve l m sp u;
      t
  | _ -> raise Unification_error
