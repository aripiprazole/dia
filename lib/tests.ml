open Parse_dia

module Syntax_tests = struct
  let run_test code =
    match parse_file code with
    | Ok v -> print_endline (Concrete.show_program v)
    | Error err -> print_endline err

  let%expect_test "pattern matching" =
    run_test {| let Nat := Set |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           (Symbol.S_symbol ((Symbol.K_prefix "Nat"),
              { Loc.file = ""; start = 0; ending = 0 }));
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           (Concrete.Expr.E_var
              (Symbol.S_symbol ((Symbol.K_prefix "Set"),
                 { Loc.file = ""; start = 0; ending = 0 })))}
          ]} |}]

  let%expect_test "arrow function" =
    run_test {| let Nat := Set -> Set |};
    [%expect
      {|
      Concrete.Program {hashbang = None;
        declarations =
        [Concrete.Top_level.T_let_decl {
           name =
           (Symbol.S_symbol ((Symbol.K_prefix "Nat"),
              { Loc.file = ""; start = 0; ending = 0 }));
           parameters = []; tt = Concrete.Expr.E_hole;
           value =
           Concrete.Expr.E_pi {
             domain =
             (Concrete.Expr.E_var
                (Symbol.S_symbol ((Symbol.K_prefix "Set"),
                   { Loc.file = ""; start = 0; ending = 0 })));
             codomain =
             (Concrete.Expr.E_var
                (Symbol.S_symbol ((Symbol.K_prefix "Set"),
                   { Loc.file = ""; start = 0; ending = 0 })))}}
          ]} |}]

  let%expect_test "pattern matching" =
    run_test
      {| let Nat :=
          (value : Nat)
          -> (P : Nat -> Set)
          -> (fzero : P zero)
          -> (fsuc  : (pred : Nat) -> P (succ pred))
          -> P value
          |};
    [%expect
      {|
      <YOUR SYNTAX ERROR MESSAGE HERE>

      Syntax error at line 3, column 23: <YOUR SYNTAX ERROR MESSAGE HERE> |}]

  let%expect_test "parse inductive naturals" =
    run_test
      {| type Nat :=
         | zero : Nat
         | succ : (pred : Nat) -> Nat |};
    [%expect
      {|
      <YOUR SYNTAX ERROR MESSAGE HERE>

      Syntax error at line 2, column 17: <YOUR SYNTAX ERROR MESSAGE HERE> |}]
end
