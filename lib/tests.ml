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
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              (Concrete.Expr.E_var
                 Symbol.S_symbol {kind = Symbol.K_prefix; text = "Set";
                   pos = Loc.Nowhere}),
              Loc.Location {startpos = :1:11; endpos = 1:14}));
           pos = Loc.Location {startpos = :1:1; endpos = 1:14}}
          ]} |}]

  let%expect_test "arrow function" =
    run_test {| let Nat = Set -> Set |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              Concrete.Expr.E_pi {
                domain =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_var
                      Symbol.S_symbol {kind = Symbol.K_prefix; text = "Set";
                        pos = Loc.Nowhere}),
                   Loc.Location {startpos = :1:11; endpos = 1:14}));
                codomain =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_var
                      Symbol.S_symbol {kind = Symbol.K_prefix; text = "Set";
                        pos = Loc.Nowhere}),
                   Loc.Location {startpos = :1:18; endpos = 1:21}))},
              Loc.Location {startpos = :1:11; endpos = 1:21}));
           pos = Loc.Location {startpos = :1:1; endpos = 1:21}}
          ]} |}]

  let%expect_test "infix : expression" =
    run_test {| let Nat = (x : Nat) |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              (Concrete.Expr.E_parens
                 (Concrete.Expr.E_src_pos (
                    Concrete.Expr.E_app {
                      callee =
                      Concrete.Expr.E_app {
                        callee =
                        (Concrete.Expr.E_var
                           Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                             pos = Loc.Nowhere});
                        arg =
                        (Concrete.Expr.E_src_pos (
                           (Concrete.Expr.E_var
                              Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                                pos = Loc.Nowhere}),
                           Loc.Location {startpos = :1:12; endpos = 1:13}))};
                      arg =
                      (Concrete.Expr.E_src_pos (
                         (Concrete.Expr.E_var
                            Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                              pos = Loc.Nowhere}),
                         Loc.Location {startpos = :1:16; endpos = 1:19}))},
                    Loc.Location {startpos = :1:12; endpos = 1:19}))),
              Loc.Location {startpos = :1:11; endpos = 1:20}));
           pos = Loc.Location {startpos = :1:1; endpos = 1:20}}
          ]} |}]

  let%expect_test "pi type" =
    run_test {| let Nat = (x : Nat) -> x |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              Concrete.Expr.E_pi {
                domain =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_parens
                      (Concrete.Expr.E_src_pos (
                         Concrete.Expr.E_app {
                           callee =
                           Concrete.Expr.E_app {
                             callee =
                             (Concrete.Expr.E_var
                                Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                                  pos = Loc.Nowhere});
                             arg =
                             (Concrete.Expr.E_src_pos (
                                (Concrete.Expr.E_var
                                   Symbol.S_symbol {kind = Symbol.K_prefix;
                                     text = "x"; pos = Loc.Nowhere}),
                                Loc.Location {startpos = :1:12; endpos = 1:13}))};
                           arg =
                           (Concrete.Expr.E_src_pos (
                              (Concrete.Expr.E_var
                                 Symbol.S_symbol {kind = Symbol.K_prefix;
                                   text = "Nat"; pos = Loc.Nowhere}),
                              Loc.Location {startpos = :1:16; endpos = 1:19}))},
                         Loc.Location {startpos = :1:12; endpos = 1:19}))),
                   Loc.Location {startpos = :1:11; endpos = 1:20}));
                codomain =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_var
                      Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                        pos = Loc.Nowhere}),
                   Loc.Location {startpos = :1:24; endpos = 1:25}))},
              Loc.Location {startpos = :1:11; endpos = 1:25}));
           pos = Loc.Location {startpos = :1:1; endpos = 1:25}}
          ]} |}]

  let%expect_test "function application" =
    run_test {| let x = f y |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "x"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              Concrete.Expr.E_app {
                callee =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_var
                      Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                        pos = Loc.Nowhere}),
                   Loc.Location {startpos = :1:9; endpos = 1:10}));
                arg =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_var
                      Symbol.S_symbol {kind = Symbol.K_prefix; text = "y";
                        pos = Loc.Nowhere}),
                   Loc.Location {startpos = :1:11; endpos = 1:12}))},
              Loc.Location {startpos = :1:9; endpos = 1:12}));
           pos = Loc.Location {startpos = :1:1; endpos = 1:12}}
          ]} |}]

  let%expect_test "function application with arrow type" =
    run_test {| let x = f x -> f y |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "x"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              Concrete.Expr.E_pi {
                domain =
                Concrete.Expr.E_app {
                  callee =
                  (Concrete.Expr.E_src_pos (
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                          pos = Loc.Nowhere}),
                     Loc.Location {startpos = :1:9; endpos = 1:10}));
                  arg =
                  (Concrete.Expr.E_src_pos (
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                          pos = Loc.Nowhere}),
                     Loc.Location {startpos = :1:11; endpos = 1:12}))};
                codomain =
                Concrete.Expr.E_app {
                  callee =
                  (Concrete.Expr.E_src_pos (
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                          pos = Loc.Nowhere}),
                     Loc.Location {startpos = :1:16; endpos = 1:17}));
                  arg =
                  (Concrete.Expr.E_src_pos (
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "y";
                          pos = Loc.Nowhere}),
                     Loc.Location {startpos = :1:18; endpos = 1:19}))}},
              Loc.Location {startpos = :1:9; endpos = 1:19}));
           pos = Loc.Location {startpos = :1:1; endpos = 1:19}}
          ]} |}]

  let%expect_test "function application with arrow type in domain" =
    run_test {| let x = (x : f x) -> f x |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "x"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              Concrete.Expr.E_pi {
                domain =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_parens
                      (Concrete.Expr.E_src_pos (
                         Concrete.Expr.E_app {
                           callee =
                           Concrete.Expr.E_app {
                             callee =
                             (Concrete.Expr.E_var
                                Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                                  pos = Loc.Nowhere});
                             arg =
                             (Concrete.Expr.E_src_pos (
                                (Concrete.Expr.E_var
                                   Symbol.S_symbol {kind = Symbol.K_prefix;
                                     text = "x"; pos = Loc.Nowhere}),
                                Loc.Location {startpos = :1:10; endpos = 1:11}))};
                           arg =
                           Concrete.Expr.E_app {
                             callee =
                             (Concrete.Expr.E_src_pos (
                                (Concrete.Expr.E_var
                                   Symbol.S_symbol {kind = Symbol.K_prefix;
                                     text = "f"; pos = Loc.Nowhere}),
                                Loc.Location {startpos = :1:14; endpos = 1:15}));
                             arg =
                             (Concrete.Expr.E_src_pos (
                                (Concrete.Expr.E_var
                                   Symbol.S_symbol {kind = Symbol.K_prefix;
                                     text = "x"; pos = Loc.Nowhere}),
                                Loc.Location {startpos = :1:16; endpos = 1:17}))}},
                         Loc.Location {startpos = :1:10; endpos = 1:17}))),
                   Loc.Location {startpos = :1:9; endpos = 1:18}));
                codomain =
                Concrete.Expr.E_app {
                  callee =
                  (Concrete.Expr.E_src_pos (
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "f";
                          pos = Loc.Nowhere}),
                     Loc.Location {startpos = :1:22; endpos = 1:23}));
                  arg =
                  (Concrete.Expr.E_src_pos (
                     (Concrete.Expr.E_var
                        Symbol.S_symbol {kind = Symbol.K_prefix; text = "x";
                          pos = Loc.Nowhere}),
                     Loc.Location {startpos = :1:24; endpos = 1:25}))}},
              Loc.Location {startpos = :1:9; endpos = 1:25}));
           pos = Loc.Location {startpos = :1:1; endpos = 1:25}}
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
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_src_pos (
              Concrete.Expr.E_pi {
                domain =
                (Concrete.Expr.E_src_pos (
                   (Concrete.Expr.E_parens
                      (Concrete.Expr.E_src_pos (
                         Concrete.Expr.E_app {
                           callee =
                           Concrete.Expr.E_app {
                             callee =
                             (Concrete.Expr.E_var
                                Symbol.S_symbol {kind = Symbol.K_infix; text = ":";
                                  pos = Loc.Nowhere});
                             arg =
                             (Concrete.Expr.E_src_pos (
                                (Concrete.Expr.E_var
                                   Symbol.S_symbol {kind = Symbol.K_prefix;
                                     text = "value"; pos = Loc.Nowhere}),
                                Loc.Location {startpos = :2:22; endpos = 2:27}))};
                           arg =
                           (Concrete.Expr.E_src_pos (
                              (Concrete.Expr.E_var
                                 Symbol.S_symbol {kind = Symbol.K_prefix;
                                   text = "Nat"; pos = Loc.Nowhere}),
                              Loc.Location {startpos = :2:30; endpos = 2:33}))},
                         Loc.Location {startpos = :2:22; endpos = 2:33}))),
                   Loc.Location {startpos = :2:21; endpos = 2:34}));
                codomain =
                Concrete.Expr.E_pi {
                  domain =
                  (Concrete.Expr.E_src_pos (
                     (Concrete.Expr.E_parens
                        (Concrete.Expr.E_src_pos (
                           Concrete.Expr.E_app {
                             callee =
                             Concrete.Expr.E_app {
                               callee =
                               (Concrete.Expr.E_var
                                  Symbol.S_symbol {kind = Symbol.K_infix;
                                    text = ":"; pos = Loc.Nowhere});
                               arg =
                               (Concrete.Expr.E_src_pos (
                                  (Concrete.Expr.E_var
                                     Symbol.S_symbol {kind = Symbol.K_prefix;
                                       text = "P"; pos = Loc.Nowhere}),
                                  Loc.Location {startpos = :3:49; endpos = 3:50}))};
                             arg =
                             Concrete.Expr.E_pi {
                               domain =
                               (Concrete.Expr.E_src_pos (
                                  (Concrete.Expr.E_var
                                     Symbol.S_symbol {kind = Symbol.K_prefix;
                                       text = "Nat"; pos = Loc.Nowhere}),
                                  Loc.Location {startpos = :3:53; endpos = 3:56}));
                               codomain =
                               (Concrete.Expr.E_src_pos (
                                  (Concrete.Expr.E_var
                                     Symbol.S_symbol {kind = Symbol.K_prefix;
                                       text = "Set"; pos = Loc.Nowhere}),
                                  Loc.Location {startpos = :3:60; endpos = 3:63}))}},
                           Loc.Location {startpos = :3:49; endpos = 3:63}))),
                     Loc.Location {startpos = :3:48; endpos = 3:64}));
                  codomain =
                  Concrete.Expr.E_pi {
                    domain =
                    (Concrete.Expr.E_src_pos (
                       (Concrete.Expr.E_parens
                          (Concrete.Expr.E_src_pos (
                             Concrete.Expr.E_app {
                               callee =
                               Concrete.Expr.E_app {
                                 callee =
                                 (Concrete.Expr.E_var
                                    Symbol.S_symbol {kind = Symbol.K_infix;
                                      text = ":"; pos = Loc.Nowhere});
                                 arg =
                                 (Concrete.Expr.E_src_pos (
                                    (Concrete.Expr.E_var
                                       Symbol.S_symbol {kind = Symbol.K_prefix;
                                         text = "fzero"; pos = Loc.Nowhere}),
                                    Loc.Location {startpos = :4:79; endpos = 4:84}
                                    ))};
                               arg =
                               Concrete.Expr.E_app {
                                 callee =
                                 (Concrete.Expr.E_src_pos (
                                    (Concrete.Expr.E_var
                                       Symbol.S_symbol {kind = Symbol.K_prefix;
                                         text = "P"; pos = Loc.Nowhere}),
                                    Loc.Location {startpos = :4:87; endpos = 4:88}
                                    ));
                                 arg =
                                 (Concrete.Expr.E_src_pos (
                                    (Concrete.Expr.E_var
                                       Symbol.S_symbol {kind = Symbol.K_prefix;
                                         text = "zero"; pos = Loc.Nowhere}),
                                    Loc.Location {startpos = :4:89; endpos = 4:93}
                                    ))}},
                             Loc.Location {startpos = :4:79; endpos = 4:93}))),
                       Loc.Location {startpos = :4:78; endpos = 4:94}));
                    codomain =
                    Concrete.Expr.E_pi {
                      domain =
                      (Concrete.Expr.E_src_pos (
                         (Concrete.Expr.E_parens
                            (Concrete.Expr.E_src_pos (
                               Concrete.Expr.E_app {
                                 callee =
                                 Concrete.Expr.E_app {
                                   callee =
                                   (Concrete.Expr.E_var
                                      Symbol.S_symbol {kind = Symbol.K_infix;
                                        text = ":"; pos = Loc.Nowhere});
                                   arg =
                                   (Concrete.Expr.E_src_pos (
                                      (Concrete.Expr.E_var
                                         Symbol.S_symbol {kind = Symbol.K_prefix;
                                           text = "fsuc"; pos = Loc.Nowhere}),
                                      Loc.Location {startpos = :5:109;
                                        endpos = 5:113}
                                      ))};
                                 arg =
                                 Concrete.Expr.E_pi {
                                   domain =
                                   (Concrete.Expr.E_src_pos (
                                      (Concrete.Expr.E_parens
                                         (Concrete.Expr.E_src_pos (
                                            Concrete.Expr.E_app {
                                              callee =
                                              Concrete.Expr.E_app {
                                                callee =
                                                (Concrete.Expr.E_var
                                                   Symbol.S_symbol {
                                                     kind = Symbol.K_infix;
                                                     text = ":"; pos = Loc.Nowhere});
                                                arg =
                                                (Concrete.Expr.E_src_pos (
                                                   (Concrete.Expr.E_var
                                                      Symbol.S_symbol {
                                                        kind = Symbol.K_prefix;
                                                        text = "pred";
                                                        pos = Loc.Nowhere}),
                                                   Loc.Location {startpos = :5:118;
                                                     endpos = 5:122}
                                                   ))};
                                              arg =
                                              (Concrete.Expr.E_src_pos (
                                                 (Concrete.Expr.E_var
                                                    Symbol.S_symbol {
                                                      kind = Symbol.K_prefix;
                                                      text = "Nat";
                                                      pos = Loc.Nowhere}),
                                                 Loc.Location {startpos = :5:125;
                                                   endpos = 5:128}
                                                 ))},
                                            Loc.Location {startpos = :5:118;
                                              endpos = 5:128}
                                            ))),
                                      Loc.Location {startpos = :5:117;
                                        endpos = 5:129}
                                      ));
                                   codomain =
                                   Concrete.Expr.E_app {
                                     callee =
                                     (Concrete.Expr.E_src_pos (
                                        (Concrete.Expr.E_var
                                           Symbol.S_symbol {kind = Symbol.K_prefix;
                                             text = "P"; pos = Loc.Nowhere}),
                                        Loc.Location {startpos = :5:133;
                                          endpos = 5:134}
                                        ));
                                     arg =
                                     (Concrete.Expr.E_src_pos (
                                        (Concrete.Expr.E_parens
                                           (Concrete.Expr.E_src_pos (
                                              Concrete.Expr.E_app {
                                                callee =
                                                (Concrete.Expr.E_src_pos (
                                                   (Concrete.Expr.E_var
                                                      Symbol.S_symbol {
                                                        kind = Symbol.K_prefix;
                                                        text = "succ";
                                                        pos = Loc.Nowhere}),
                                                   Loc.Location {startpos = :5:136;
                                                     endpos = 5:140}
                                                   ));
                                                arg =
                                                (Concrete.Expr.E_src_pos (
                                                   (Concrete.Expr.E_var
                                                      Symbol.S_symbol {
                                                        kind = Symbol.K_prefix;
                                                        text = "pred";
                                                        pos = Loc.Nowhere}),
                                                   Loc.Location {startpos = :5:141;
                                                     endpos = 5:145}
                                                   ))},
                                              Loc.Location {startpos = :5:136;
                                                endpos = 5:145}
                                              ))),
                                        Loc.Location {startpos = :5:135;
                                          endpos = 5:146}
                                        ))}}},
                               Loc.Location {startpos = :5:109; endpos = 5:146}))),
                         Loc.Location {startpos = :5:108; endpos = 5:147}));
                      codomain =
                      Concrete.Expr.E_app {
                        callee =
                        (Concrete.Expr.E_src_pos (
                           (Concrete.Expr.E_var
                              Symbol.S_symbol {kind = Symbol.K_prefix; text = "P";
                                pos = Loc.Nowhere}),
                           Loc.Location {startpos = :6:161; endpos = 6:162}));
                        arg =
                        (Concrete.Expr.E_src_pos (
                           (Concrete.Expr.E_var
                              Symbol.S_symbol {kind = Symbol.K_prefix;
                                text = "value"; pos = Loc.Nowhere}),
                           Loc.Location {startpos = :6:163; endpos = 6:168}))}}}}},
              Loc.Location {startpos = :2:21; endpos = 6:168}));
           pos = Loc.Location {startpos = :1:1; endpos = 6:168}}
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
           Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat"; pos = Loc.Nowhere};
           parameters = []; tt = Concrete.Expr.E_hole;
           constructors =
           [Concrete.Top_level.Constructor {
              name =
              Symbol.S_symbol {kind = Symbol.K_prefix; text = "zero";
                pos = Loc.Nowhere};
              tt =
              (Concrete.Expr.E_src_pos (
                 (Concrete.Expr.E_var
                    Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                      pos = Loc.Nowhere}),
                 Loc.Location {startpos = :2:30; endpos = 2:33}));
              pos = Loc.Location {startpos = :2:21; endpos = 2:33}};
             Concrete.Top_level.Constructor {
               name =
               Symbol.S_symbol {kind = Symbol.K_prefix; text = "succ";
                 pos = Loc.Nowhere};
               tt =
               (Concrete.Expr.E_src_pos (
                  Concrete.Expr.E_pi {
                    domain =
                    (Concrete.Expr.E_src_pos (
                       (Concrete.Expr.E_parens
                          (Concrete.Expr.E_src_pos (
                             Concrete.Expr.E_app {
                               callee =
                               Concrete.Expr.E_app {
                                 callee =
                                 (Concrete.Expr.E_var
                                    Symbol.S_symbol {kind = Symbol.K_infix;
                                      text = ":"; pos = Loc.Nowhere});
                                 arg =
                                 (Concrete.Expr.E_src_pos (
                                    (Concrete.Expr.E_var
                                       Symbol.S_symbol {kind = Symbol.K_prefix;
                                         text = "pred"; pos = Loc.Nowhere}),
                                    Loc.Location {startpos = :3:53; endpos = 3:57}
                                    ))};
                               arg =
                               (Concrete.Expr.E_src_pos (
                                  (Concrete.Expr.E_var
                                     Symbol.S_symbol {kind = Symbol.K_prefix;
                                       text = "Nat"; pos = Loc.Nowhere}),
                                  Loc.Location {startpos = :3:60; endpos = 3:63}))},
                             Loc.Location {startpos = :3:53; endpos = 3:63}))),
                       Loc.Location {startpos = :3:52; endpos = 3:64}));
                    codomain =
                    (Concrete.Expr.E_src_pos (
                       (Concrete.Expr.E_var
                          Symbol.S_symbol {kind = Symbol.K_prefix; text = "Nat";
                            pos = Loc.Nowhere}),
                       Loc.Location {startpos = :3:68; endpos = 3:71}))},
                  Loc.Location {startpos = :3:52; endpos = 3:71}));
               pos = Loc.Location {startpos = :3:43; endpos = 3:71}}
             ];
           pos = Loc.Location {startpos = :1:1; endpos = 3:71}}
          ]} |}]
end
