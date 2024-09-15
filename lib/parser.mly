%{
  open Syntax
%}

%token <int> NUMBER
%token <string> ID
%token <string> INFIX_ID
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token LEFT_BRACES (* { *)
%token RIGHT_BRACES (* } *)
%token DOT (* . *)
%token ARROW (* -> *)
%token DOUBLE_ARROW (* => *)
%token MATCH (* match *)
%token WITH (* with *)
%token IN (* in *)
%token EQUALS (* = *)
%token DEF_EQUALS (* := *)
%token COLON (* : *)
%token LET (* let *)
%token PRAGMA (* #pragma *)
%token BAR (* | *)
%token FUN (* fun *)
%token INDUCTIVE (* inductive *)
%token COMMA (* , *)

%token EOF

%start <program> file

%%

let symbol := name = ID; { Symbol.make name }
let infix_symbol := name = INFIX_ID; { Symbol.make name }

let expr :=
  | INDUCTIVE; cs = list(constructor); { Expr.Inductive cs }
  | FUN; ps = nonempty_list(symbol); ARROW; e = expr; { e_lam ps e }
  | MATCH; scrutinee = expr; WITH; cases = list(case); { e_match scrutinee cases }
  | LET; name = symbol; DEF_EQUALS; value = expr; IN; body = expr; { e_let name value body }
  | e_pi
  | e_infix

let constructor := BAR; name = def_name; tt = type_repr; { Expr.Constructor { name; tt; } }

let file :=
  | decls = list(decl); EOF; { Program { hashbang = None; declarations = decls } }

let parameter :=
  | LEFT_PARENS; names = nonempty_list(symbol); tt = option(type_repr); RIGHT_PARENS; { Expr.Parameter { names; tt = Option.value ~default:Expr.Hole tt; icit = Expl } }
  | LEFT_BRACES; names = nonempty_list(symbol); tt = option(type_repr); RIGHT_BRACES; { Expr.Parameter { names; tt = Option.value ~default:Expr.Hole tt; icit = Impl } }

let def_name :=
  | name = symbol; { Prefix name }
  | LEFT_PARENS; name = symbol; RIGHT_PARENS; { Infix name }

let type_repr := COLON; tt = tt; { tt }

let decl :=
  | LET; name = def_name; parameters = list(parameter); tt = option(type_repr); DEF_EQUALS; value = expr;
    { TopLevel.Definition { name; parameters; tt = Option.value ~default:Expr.Hole tt; value } }

let case := BAR; p = pattern; DOUBLE_ARROW; e = primary; { (p, e) }

let pattern :=
  | name = symbol; { Pattern.Var name }
  | LEFT_PARENS; name = def_name; args = list(pattern); RIGHT_PARENS; { Pattern.Constructor { name; args } }

let e_app :=
  | primary
  | callee = e_app; arg = primary; { e_app callee Expl arg }
  | callee = e_app; LEFT_BRACES; arg = expr; RIGHT_BRACES; { e_app callee Impl arg }

let e_infix := lhs = primary; op = infix_symbol; rhs = primary; { curry (Expr.Var op) [(Expl, lhs); (Expl, rhs)] }

let e_pi :=
  | e_app
  | d = e_pi; ARROW; c = primary; { e_pi d c }

let tt := e_pi | e_infix

let primary :=
  | n = NUMBER; { Expr.Num n }
  | name = symbol; { Expr.Var name }
  | LEFT_PARENS; expr = expr; RIGHT_PARENS; { expr }
