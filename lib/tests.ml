open Parse_dia

module Syntax_tests = struct
  let run_test code =
    match parse_file code with
    | Ok v -> print_endline (Concrete.show_program v)
    | Error err -> print_endline err

  let%expect_test "simple let" =
    run_test {| let Nat = Set |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_var
              Symbol.S_symbol {kind = Symbol.K_prefix; text = "Set";
                pos = { Loc.file = ""; start = 0; ending = 0 }})}
          ]} |}]

  let%expect_test "arrow function" =
    run_test {| let Nat = Set -> Set |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           Concrete.Expr.E_pi {
             domain =
             (Concrete.Expr.E_var
                Symbol.S_symbol {kind = Symbol.K_prefix; text = "Set";
                  pos = { Loc.file = ""; start = 0; ending = 0 }});
             codomain =
             (Concrete.Expr.E_var
                Symbol.S_symbol {kind = Symbol.K_prefix; text = "Set";
                  pos = { Loc.file = ""; start = 0; ending = 0 }})}}
          ]} |}]

  let%expect_test "infix : expression" =
    run_test {| let Nat = (x : Nat) |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_parens
              Concrete.Expr.E_app {
                callee =
                Concrete.Expr.E_app {
                  callee =
                  (Concrete.Expr.E_var
                     Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                       pos = { Loc.file = ""; start = 0; ending = 0 }});
                  arg =
                  (Concrete.Expr.E_var
                     Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                       pos = { Loc.file = ""; start = 0; ending = 0 }})};
                arg =
                (Concrete.Expr.E_var
                   Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                     pos = { Loc.file = ""; start = 0; ending = 0 }})})}
          ]} |}]

  let%expect_test "pi type" =
    run_test {| let Nat = (x : Nat) -> x |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           Concrete.Expr.E_pi {
             domain =
             (Concrete.Expr.E_parens
                Concrete.Expr.E_app {
                  callee =
                  Concrete.Expr.E_app {
                    callee =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                         pos = { Loc.file = ""; start = 0; ending = 0 }});
                    arg =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                         pos = { Loc.file = ""; start = 0; ending = 0 }})};
                  arg =
                  (Concrete.Expr.E_var
                     Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                       pos = { Loc.file = ""; start = 0; ending = 0 }})});
             codomain =
             (Concrete.Expr.E_var
                Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                  pos = { Loc.file = ""; start = 0; ending = 0 }})}}
          ]} |}]

  let%expect_test "function application" =
    run_test {| let x = f y |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           Concrete.Expr.E_app {
             callee =
             (Concrete.Expr.E_var
                Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                  pos = { Loc.file = ""; start = 0; ending = 0 }});
             arg =
             (Concrete.Expr.E_var
                Symbol.S_symbol {kind = Symbol.K_prefix; text = "y";
                  pos = { Loc.file = ""; start = 0; ending = 0 }})}}
          ]} |}]

  let%expect_test "function application with arrow type" =
    run_test {| let x = f x -> f y |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           Concrete.Expr.E_pi {
             domain =
             Concrete.Expr.E_app {
               callee =
               (Concrete.Expr.E_var
                  Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                    pos = { Loc.file = ""; start = 0; ending = 0 }});
               arg =
               (Concrete.Expr.E_var
                  Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                    pos = { Loc.file = ""; start = 0; ending = 0 }})};
             codomain =
             Concrete.Expr.E_app {
               callee =
               (Concrete.Expr.E_var
                  Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                    pos = { Loc.file = ""; start = 0; ending = 0 }});
               arg =
               (Concrete.Expr.E_var
                  Symbol.S_symbol {kind = Symbol.K_prefix; text = "y";
                    pos = { Loc.file = ""; start = 0; ending = 0 }})}}}
          ]} |}]

  let%expect_test "function application with arrow type in domain" =
    run_test {| let x = (x : f x) -> f x |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           Concrete.Expr.E_pi {
             domain =
             (Concrete.Expr.E_parens
                Concrete.Expr.E_app {
                  callee =
                  Concrete.Expr.E_app {
                    callee =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                         pos = { Loc.file = ""; start = 0; ending = 0 }});
                    arg =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                         pos = { Loc.file = ""; start = 0; ending = 0 }})};
                  arg =
                  Concrete.Expr.E_app {
                    callee =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                         pos = { Loc.file = ""; start = 0; ending = 0 }});
                    arg =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                         pos = { Loc.file = ""; start = 0; ending = 0 }})}});
             codomain =
             Concrete.Expr.E_app {
               callee =
               (Concrete.Expr.E_var
                  Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                    pos = { Loc.file = ""; start = 0; ending = 0 }});
               arg =
               (Concrete.Expr.E_var
                  Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                    pos = { Loc.file = ""; start = 0; ending = 0 }})}}}
          ]} |}]

  let%expect_test "pattern matching" =
    run_test
      {| let Nat =
          (value : Nat)
          -> (P : Nat -> Set)
          -> (fzero : P zero)
          -> (fsuc  : (pred : Nat) -> P (succ pred))
          -> P value |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           Concrete.Expr.E_pi {
             domain =
             (Concrete.Expr.E_parens
                Concrete.Expr.E_app {
                  callee =
                  Concrete.Expr.E_app {
                    callee =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                         pos = { Loc.file = ""; start = 0; ending = 0 }});
                    arg =
                    (Concrete.Expr.E_var
                       Symbol.S_symbol {kind = Symbol.K_prefix; text = "value";
                         pos = { Loc.file = ""; start = 0; ending = 0 }})};
                  arg =
                  (Concrete.Expr.E_var
                     Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                       pos = { Loc.file = ""; start = 0; ending = 0 }})});
             codomain =
             Concrete.Expr.E_pi {
               domain =
               (Concrete.Expr.E_parens
                  Concrete.Expr.E_app {
                    callee =
                    Concrete.Expr.E_app {
                      callee =
                      (Concrete.Expr.E_var
                         Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                           pos = { Loc.file = ""; start = 0; ending = 0 }});
                      arg =
                      (Concrete.Expr.E_var
                         Symbol.S_symbol {kind = Symbol.K_prefix; text = "P";
                           pos = { Loc.file = ""; start = 0; ending = 0 }})};
                    arg =
                    Concrete.Expr.E_pi {
                      domain =
                      (Concrete.Expr.E_var
                         Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                           pos = { Loc.file = ""; start = 0; ending = 0 }});
                      codomain =
                      (Concrete.Expr.E_var
                         Symbol.S_symbol {kind = Symbol.K_prefix; text = "Set";
                           pos = { Loc.file = ""; start = 0; ending = 0 }})}});
               codomain =
               Concrete.Expr.E_pi {
                 domain =
                 (Concrete.Expr.E_parens
                    Concrete.Expr.E_app {
                      callee =
                      Concrete.Expr.E_app {
                        callee =
                        (Concrete.Expr.E_var
                           Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                             pos = { Loc.file = ""; start = 0; ending = 0 }});
                        arg =
                        (Concrete.Expr.E_var
                           Symbol.S_symbol {kind = Symbol.K_prefix; text = "fzero";
                             pos = { Loc.file = ""; start = 0; ending = 0 }})};
                      arg =
                      Concrete.Expr.E_app {
                        callee =
                        (Concrete.Expr.E_var
                           Symbol.S_symbol {kind = Symbol.K_prefix; text = "P";
                             pos = { Loc.file = ""; start = 0; ending = 0 }});
                        arg =
                        (Concrete.Expr.E_var
                           Symbol.S_symbol {kind = Symbol.K_prefix; text = "zero";
                             pos = { Loc.file = ""; start = 0; ending = 0 }})}});
                 codomain =
                 Concrete.Expr.E_pi {
                   domain =
                   (Concrete.Expr.E_parens
                      Concrete.Expr.E_app {
                        callee =
                        Concrete.Expr.E_app {
                          callee =
                          (Concrete.Expr.E_var
                             Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                               pos = { Loc.file = ""; start = 0; ending = 0 }});
                          arg =
                          (Concrete.Expr.E_var
                             Symbol.S_symbol {kind = Symbol.K_prefix;
                               text = "fsuc";
                               pos = { Loc.file = ""; start = 0; ending = 0 }})};
                        arg =
                        Concrete.Expr.E_pi {
                          domain =
                          (Concrete.Expr.E_parens
                             Concrete.Expr.E_app {
                               callee =
                               Concrete.Expr.E_app {
                                 callee =
                                 (Concrete.Expr.E_var
                                    Symbol.S_symbol {kind = Symbol.K_infix;
                                      text = ":";
                                      pos =
                                      { Loc.file = ""; start = 0; ending = 0 }});
                                 arg =
                                 (Concrete.Expr.E_var
                                    Symbol.S_symbol {kind = Symbol.K_prefix;
                                      text = "pred";
                                      pos =
                                      { Loc.file = ""; start = 0; ending = 0 }})};
                               arg =
                               (Concrete.Expr.E_var
                                  Symbol.S_symbol {kind = Symbol.K_prefix;
                                    text = "Nat";
                                    pos = { Loc.file = ""; start = 0; ending = 0 }})});
                          codomain =
                          Concrete.Expr.E_app {
                            callee =
                            (Concrete.Expr.E_var
                               Symbol.S_symbol {kind = Symbol.K_prefix; text = "P";
                                 pos = { Loc.file = ""; start = 0; ending = 0 }});
                            arg =
                            (Concrete.Expr.E_parens
                               Concrete.Expr.E_app {
                                 callee =
                                 (Concrete.Expr.E_var
                                    Symbol.S_symbol {kind = Symbol.K_prefix;
                                      text = "succ";
                                      pos =
                                      { Loc.file = ""; start = 0; ending = 0 }});
                                 arg =
                                 (Concrete.Expr.E_var
                                    Symbol.S_symbol {kind = Symbol.K_prefix;
                                      text = "pred";
                                      pos =
                                      { Loc.file = ""; start = 0; ending = 0 }})})}}});
                   codomain =
                   Concrete.Expr.E_app {
                     callee =
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "P";
                          pos = { Loc.file = ""; start = 0; ending = 0 }});
                     arg =
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "value";
                          pos = { Loc.file = ""; start = 0; ending = 0 }})}}}}}}
          ]} |}]

  let%expect_test "parse inductive naturals" =
    run_test
      {| type Nat =
         | zero : Nat
         | succ : (pred : Nat) -> Nat |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_type_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
             pos = { Loc.file = ""; start = 0; ending = 0 }};
           parameters = []; tt = Concrete.Expr.E_hole;
           constructors =
           [Concrete.Top_level.Constructor {
              name =
              Symbol.S_symbol {kind = Symbol.K_prefix; text = "zero";
                pos = { Loc.file = ""; start = 0; ending = 0 }};
              tt =
              (Concrete.Expr.E_var
                 Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                   pos = { Loc.file = ""; start = 0; ending = 0 }})};
             Concrete.Top_level.Constructor {
               name =
               Symbol.S_symbol {kind = Symbol.K_prefix; text = "succ";
                 pos = { Loc.file = ""; start = 0; ending = 0 }};
               tt =
               Concrete.Expr.E_pi {
                 domain =
                 (Concrete.Expr.E_parens
                    Concrete.Expr.E_app {
                      callee =
                      Concrete.Expr.E_app {
                        callee =
                        (Concrete.Expr.E_var
                           Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                             pos = { Loc.file = ""; start = 0; ending = 0 }});
                        arg =
                        (Concrete.Expr.E_var
                           Symbol.S_symbol {kind = Symbol.K_prefix; text = "pred";
                             pos = { Loc.file = ""; start = 0; ending = 0 }})};
                      arg =
                      (Concrete.Expr.E_var
                         Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                           pos = { Loc.file = ""; start = 0; ending = 0 }})});
                 codomain =
                 (Concrete.Expr.E_var
                    Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                      pos = { Loc.file = ""; start = 0; ending = 0 }})}}
             ]}
          ]} |}]
end
