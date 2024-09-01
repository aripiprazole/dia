Require Import Coq.Strings.String.

Inductive expr : Type :=
| fvar : string -> expr
| bvar : nat -> expr
| lam : expr -> expr
| app : expr -> expr -> expr
| zero : expr
| succ : expr -> expr
| mat : expr -> expr -> expr -> expr
| fixp : string -> expr -> expr.

Notation "`µ s e" := (fixp s e) (at level 90).
Notation "' n" := (fvar n) (at level 50).
Notation "` n" := (bvar n) (at level 50).
Notation "ƛ. e" := (lam e) (at level 50).
Notation "e1 · e2" := (app e1 e2) (at level 50).
Notation "'case' e1 ⟦ `succ ⇒ e2 | `zero ⇒ e3 ⟧" := (mat e1 e2 e3) (at level 50).

Definition two := succ (succ zero).

Definition plus := `µ "+" ƛ. ƛ. case `1
  ⟦ `succ ⇒ succ (' "+" ·  `0 · `1)
  | `zero ⇒ zero ⟧.

Inductive value : expr -> Type :=
| v_lam : forall n, value (lam n)
| v_zero : value zero
| v_succ : forall e, value e -> value (succ e).

Reserved Notation "a ⟹ b" (at level 50).

Inductive to : expr -> expr -> Type :=
| xi_app_1 : forall l l' m, l ⟹ l' -> (l · m) ⟹ (l' · m)
where "a ⟹ b" := (to a b) (at level 50).


