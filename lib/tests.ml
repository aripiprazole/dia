open Parse_dia

module Syntax_tests = struct
  let run_test code =
    print_endline
      (match parse_file code with
      | Ok v -> Concrete.show_program v
      | Error err -> err)

  let%expect_test "pattern matching" =
    run_test
      {| let Nat =
          (value : Nat)
          -> (P : Nat -> Set)
          -> (fzero : P zero)
          -> (fsuc  : (pred : Nat) -> P (succ pred))
          -> P value
          |};
    [%expect {| |}]

  let%expect_test "parse inductive naturals" =
    run_test
      {| let Nat = inductive
         | zero : Nat
         | succ : (pred : Nat) -> Nat |};
    [%expect {| |}]
end
