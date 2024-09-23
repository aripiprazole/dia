open Term
open Format

let icit_to_pi_prefix = function
| Concrete.Impl -> ""
| Concrete.Expl -> "∀"

let rec pprint prec ns = function
| T_src_pos { value; _ } -> pprint prec ns value
| T_lam (_, _, cod) as l ->
    let params = lam_dom l |> List.map pp_lam_param |> String.concat " " in
    let cod = Option.value (lam_cod l) ~default:cod in
    sprintf "λ %s. %s" params (pprint prec ns cod)
| T_bvar (Debruijin.Idx { value; _ }) -> List.nth ns value
| T_hole (Meta_var m) -> sprintf "?%i" m
| T_app (callee, sp) ->
    List.fold_left
      (fun acc -> function
        | next, Concrete.Impl -> sprintf "%s {%s}" acc (pprint prec ns next)
        | next, Concrete.Expl -> sprintf "%s {%s}" acc (pprint prec ns next))
      (pprint prec ns callee) sp
| T_u -> "★"
| T_pi (Dom { icit; _ }, cod) as p ->
    let params =
      pi_dom p |> List.map (pp_pi_param prec ns) |> String.concat " "
    in
    let dom = sprintf "%s %s" (icit_to_pi_prefix icit) params in
    let cod = Option.value (pi_cod p) ~default:cod in
    sprintf "%s → %s" dom (pprint prec ns cod)
| _ -> assert false

and pp_pi_param prec ns = function
| Dom { name = Some name; icit = Concrete.Expl; dom } ->
    sprintf "(%s : %s)" (Loc.unwrap name) (pprint prec ns dom)
| Dom { name = Some name; icit = Concrete.Impl; dom } ->
    sprintf "{%s : %s}" (Loc.unwrap name) (pprint prec ns dom)
| Dom { name = None; icit = Concrete.Expl; dom } ->
    sprintf "%s" (pprint prec ns dom)
| Dom { name = None; icit = Concrete.Impl; dom } ->
    sprintf "{%s}" (pprint prec ns dom)

and pp_lam_param = function
| Some name, Concrete.Expl -> sprintf "%s" (Loc.unwrap name)
| Some name, Concrete.Impl -> sprintf "{%s}" (Loc.unwrap name)
| None, Concrete.Expl -> "_"
| None, Concrete.Impl -> "{_}"
