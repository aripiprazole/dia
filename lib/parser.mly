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
%token DEF_EQUALS (* := *)
%token COLON (* : *)
%token LET (* let *)
%token TYPE (* type *)
%token PRAGMA (* #pragma *)
%token BAR (* | *)
%token FUN (* fun *)

%token EOF

%start <program> file

%%

let symbol := name = ID; { Symbol.make (K_prefix name) }
let infix_symbol := name = INFIX_ID; { Symbol.make (K_infix name) }

let expr :=
  | FUN; ps = nonempty_list(symbol); ARROW; e = expr; { e_lam ps e }
  | MATCH; scrutinee = expr; WITH; cases = list(case); { e_match scrutinee cases }
  | LET; name = symbol; DEF_EQUALS; value = expr; IN; body = expr; { e_let name value body }
  | e_pi
  | e_infix

let constructor := BAR; name = def_name; tt = type_repr; { Constructor { name; tt; } }

let file :=
  | decls = list(decl); EOF; { Program { hashbang = None; declarations = decls } }

let parameter :=
  | LEFT_PARENS; names = nonempty_list(symbol); tt = option(type_repr); RIGHT_PARENS; { Parameter { names; tt = Option.value ~default:E_hole tt; icit = Expl } }
  | LEFT_BRACES; names = nonempty_list(symbol); tt = option(type_repr); RIGHT_BRACES; { Parameter { names; tt = Option.value ~default:E_hole tt; icit = Impl } }

let def_name :=
  | name = symbol; { Symbol.map (fun s -> K_prefix s) name }
  | LEFT_PARENS; name = symbol; RIGHT_PARENS; { Symbol.map (fun s -> K_infix s) name }

let type_repr := COLON; tt = tt; { tt }

let decl :=
  | LET; name = def_name; parameters = list(parameter); tt = option(type_repr); DEF_EQUALS; value = expr;
    { T_let_decl { name; parameters; tt = Option.value ~default:E_hole tt; value } }
  | TYPE; name = def_name; parameters = list(parameter); tt = option(type_repr); DEF_EQUALS; constructors = list(constructor);
    { T_type_decl { name; parameters; tt = Option.value ~default:E_hole tt; constructors } }

let case := BAR; p = pattern; DOUBLE_ARROW; e = primary; { (p, e) }

let pattern :=
  | name = symbol; { P_var name }
  | LEFT_PARENS; name = def_name; args = list(pattern); RIGHT_PARENS; { P_constructor { name; args } }

let e_app :=
  | primary
  | callee = e_app; arg = primary; { e_app callee Expl arg }
  | callee = e_app; LEFT_BRACES; arg = expr; RIGHT_BRACES; { e_app callee Impl arg }

let e_infix := lhs = primary; op = infix_symbol; rhs = primary; { curry (E_var op) [(Expl, lhs); (Expl, rhs)] }

let e_pi :=
  | e_app
  | d = e_app; ARROW; c = e_pi; { e_pi d c }

let tt := e_pi | e_infix

let primary :=
  | n = NUMBER; { E_num n }
  | name = symbol; { E_var name }
  | LEFT_PARENS; expr = expr; RIGHT_PARENS; { expr }
