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
%token HYPOTHESIS (* - *)
%token MATCH (* match *)
%token WITH (* with *)
%token IN (* in *)
%token EQUALS (* = *)
%token COLON (* : *)
%token QUESTION_MARK (* ? *)
%token LET (* let *)
%token PRAGMA (* #pragma *)
%token SET (* Set *)
%token BAR (* | *)
%token FUN (* fun *)

%token EOF

%start <program> file

%%

let symbol := name = ID; { Symbol.make name }
let infix_symbol := name = INFIX_ID; { Symbol.make name }

let expr :=
  | constructors = nonempty_list(constructor);
    { Expr.Inductive constructors }
  | SET;
    { Expr.U }
  | p = parameter; ARROW; option(nonempty_list(HYPOTHESIS)); e = expr;
    { e_pi p e }
  | FUN; ps = nonempty_list(symbol); ARROW; e = expr;
    { e_lam ps e }
  | callee = expr; arg = expr;
    { e_app callee Expl arg }
  | callee = expr; LEFT_BRACES; arg = expr; RIGHT_BRACES;
    { e_app callee Impl arg }
  | MATCH; scrutinee = expr; WITH; option(BAR); cases = separated_nonempty_list(BAR, case);
    { e_match scrutinee cases }
  | expr_atom

let constructor :=
  | BAR; name = def_name; COLON; tt = expr;
    { Expr.Constructor { name; tt; } }

let file :=
  | EOF;
    { Program { hashbang = None; declarations = [] } }
  | decls = nonempty_list(decl); EOF;
    { Program { hashbang = None; declarations = decls } }

let name :=
  | name = ID;
    { Loc.{ value = name; pos = Loc.synthesized } }

let parameter :=
  | LEFT_PARENS; names = nonempty_list(name); COLON; tt = expr; RIGHT_PARENS;
    { Expr.Parameter { names; tt; icit = Expl } }
  | LEFT_BRACES; names = nonempty_list(name); COLON; tt = expr; RIGHT_BRACES;
    { Expr.Parameter { names; tt; icit = Impl } }

let def_name :=
  | name = name; { Prefix name }
  | LEFT_PARENS; name = name; RIGHT_PARENS; { Infix name }

let decl :=
  | LET; name = def_name; COLON; tt = option(expr); EQUALS; value = expr;
    { TopLevel.Definition { name; parameters = []; tt = Option.value ~default:(Expr.Hole None) tt; value } }
  | LET; name = def_name; parameters = nonempty_list(parameter); COLON; tt = option(expr); EQUALS; value = expr;
    { TopLevel.Definition { name; parameters; tt = Option.value ~default:(Expr.Hole None) tt; value } }

let case := pattern = pattern; DOUBLE_ARROW; expr = expr; { (pattern, expr) }

let pattern :=
  | name = symbol;
    { Pattern.Var name }
  | LEFT_PARENS; name = def_name; args = list(pattern); RIGHT_PARENS;
    { Pattern.Constructor { name; args } }

let expr_atom :=
  | n = NUMBER;
    { Expr.Num n }
  | lhs = expr; name = infix_symbol; rhs = expr;
    { e_app (e_app (Expr.Var name) Expl lhs) Expl rhs }
  | QUESTION_MARK; name = symbol;
    { Expr.Hole name }
  | LET; name = symbol; EQUALS; value = expr; IN; body = expr;
    { e_let name value body }
  | LEFT_PARENS; expr = expr; RIGHT_PARENS;
    { expr }
