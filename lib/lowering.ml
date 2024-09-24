open Concrete.Expr

type lowering_ctx = {
  pos : Loc.t;
  functions : (string, Abstract.Expr.t) Hashtbl.t;
}

let rec lower_expr ctx = function
| E_src_pos (value, pos) -> lower_expr { ctx with pos } value
| E_u
| E_hole
| E_num _
| E_parens _
| E_braces _
| E_var _
| E_lam _
| E_app _
| E_pi _
| E_let _
| E_match _ ->
    assert false
