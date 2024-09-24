open Concrete.Expr

type lowering_ctx = {
  pos : Loc.t;
  functions : (string, Abstract.Expr.t) Hashtbl.t;
}

let expect_var _ = function
| E_var name -> name
| _ -> failwith "can't find the string"

let rec lower_expr ctx = function
| E_src_pos (value, pos) -> lower_expr { ctx with pos } value
| E_pi { domain; codomain } ->
    let domain = lower_domain ctx domain in
    let codomain = lower_expr ctx codomain in
    Abstract.Expr.E_pi (domain, codomain)
| E_u
| E_hole
| E_as _
| E_num _
| E_parens _
| E_braces _
| E_var _
| E_lam _
| E_app _
| E_let _
| E_match _ ->
    assert false

and lower_domain ctx = function
| E_src_pos (value, pos) -> lower_domain { ctx with pos } value
| E_parens (E_src_pos (E_as (name, type_repr), _)) ->
    let name = expect_var ctx name in
    let type_repr = lower_expr ctx type_repr in
    Abstract.Expr.Dom (name, Concrete.Expl, type_repr)
| E_braces (E_src_pos (E_as (name, type_repr), _)) ->
    let name = expect_var ctx name in
    let type_repr = lower_expr ctx type_repr in
    Abstract.Expr.Dom (name, Concrete.Impl, type_repr)
| expr ->
    let name = Symbol.make Symbol.K_hole "_" in
    let type_repr = lower_expr ctx expr in
    Abstract.Expr.Dom (name, Concrete.Expl, type_repr)
