%{
  open Concrete
  open Concrete.Top_level
  open Concrete.Expr
  open Concrete.Pattern
  open Symbol
%}

%token <int> NUMBER
%token <string> ID
%token <string> INFIX_ID
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token LEFT_BRACES (* { *)
%token RIGHT_BRACES (* } *)
%token ARROW (* -> *)
%token DOUBLE_ARROW (* => *)
%token MATCH (* match *)
%token WITH (* with *)
%token IN (* in *)
%token EQUALS (* = *)
%token COLON (* : *)
%token LET (* let *)
%token TYPE (* type *)
%token PRAGMA (* #pragma *)
%token BAR (* | *)
%token FUN (* fun *)

%token EOF

%start <program> file

%%

symbol: name = ID; { Symbol.make ~pos:(Loc.make_location $startpos $endpos) K_prefix name }
infix_symbol:
  | name = INFIX_ID; { Symbol.make ~pos:(Loc.make_location $startpos $endpos) K_infix name }
  | COLON; { Symbol.make ~pos:(Loc.make_location $startpos $endpos) K_infix ":" }

constructor: BAR; name = def_name; tt = type_repr; { Constructor { name; tt; pos = Loc.make_location $startpos $endpos } }

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | FUN; ps = nonempty_list(symbol); ARROW; e = expr;
    { e_lam ps e }
  | MATCH; scrutinee = expr; WITH; cases = list(case);
    { e_match scrutinee cases }
  | LET; name = symbol; EQUALS; value = expr; IN; body = expr;
    { e_let name value body }
  | e = e_pi { e }
  | e = e_infix { e }

file: decls = list(decl); EOF; { Program { hashbang = None; declarations = decls } }

parameter:
  | LEFT_PARENS; names = nonempty_list(symbol); tt = option(type_repr); RIGHT_PARENS;
    { Parameter { names; tt = Option.value ~default:E_hole tt; icit = Expl } }
  | LEFT_BRACES; names = nonempty_list(symbol); tt = option(type_repr); RIGHT_BRACES;
    { Parameter { names; tt = Option.value ~default:E_hole tt; icit = Impl } }

def_name:
  | name = symbol;
    { Symbol.make ~pos:(Loc.make_location $startpos $endpos) K_prefix (text name) }
  | LEFT_PARENS; name = symbol; RIGHT_PARENS;
    { Symbol.make ~pos:(Loc.make_location $startpos $endpos) K_infix (text name) }

type_repr: COLON; tt = tt; { tt }

decl:
  | LET; name = def_name; parameters = list(parameter); tt = option(type_repr); EQUALS; value = expr;
    { T_let_decl { name; parameters; tt = Option.value ~default:E_hole tt; value; pos = Loc.make_location $startpos $endpos } }
  | TYPE; name = def_name; parameters = list(parameter); tt = option(type_repr); EQUALS; constructors = list(constructor);
    { T_type_decl { name; parameters; tt = Option.value ~default:E_hole tt; constructors; pos = Loc.make_location $startpos $endpos } }
  | PRAGMA; name = def_name; arguments = list(def_name);
    { T_pragma { name; arguments; pos = Loc.make_location $startpos $endpos } }

case:
  | BAR; p = pattern; DOUBLE_ARROW; e = primary;
    { (p, e) }

pattern:
  | name = symbol;
    { P_var name }
  | LEFT_PARENS; name = def_name; args = list(pattern); RIGHT_PARENS;
    { P_constructor { name; args } }

e_app:
  | e = primary { e }
  | callee = e_app; arg = primary; { E_app { callee; arg } }

e_infix:
  | lhs = e_pi; op = infix_symbol; rhs = e_pi;
    { E_app { callee = E_app { callee = E_var op; arg = lhs }; arg = rhs } }

e_pi:
  | e = e_app { e }
  | domain = e_app; ARROW; codomain = e_pi; { E_pi { domain; codomain } }

tt: mark_position(plain_tt) { $1 }
plain_tt: e = e_pi { e } | e = e_infix { e }

primary: mark_position(plain_primary) { $1 }
plain_primary:
  | n = NUMBER; { E_num n }
  | name = symbol; { E_var name }
  | LEFT_BRACES; expr = expr; RIGHT_BRACES; { E_braces expr }
  | LEFT_PARENS; expr = expr; RIGHT_PARENS; { E_parens expr }

mark_position(X):
  x = X;
  { match x with | E_src_pos (v, s) -> E_src_pos (v, s) | _ -> E_src_pos (x, Loc.make_location $startpos $endpos) }
