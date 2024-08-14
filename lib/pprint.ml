open Term
open Format

let rec pprint prec ns = function
| Src_pos { value; _ } -> pprint prec ns value
| Lam _ as l ->
    let params = lam_dom [] l |> List.map pp_lam_param |> String.concat " " in
    let cod = Option.value (lam_cod l) ~default:l in
    sprintf "λ %s. %s" params (pprint prec ns cod)
| _ -> assert false

and pp_lam_param = function
| Some name, Core.Expl -> sprintf "%s" (Loc.unwrap name)
| Some name, Core.Impl -> sprintf "{%s}" (Loc.unwrap name)
| None, Core.Expl -> sprintf "_"
| None, Core.Impl -> sprintf "{_}"
