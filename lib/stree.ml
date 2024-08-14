(* Pat φ = refl | () | x φ₀..φₙ *)
type pat =
  | P_refl
  | P_absurd
  | P_cons of Symbol.t * pat list

(* Param p = {x} | x *)
type param = Param of Symbol.t * Core.icit

(* Expr m n α τ =
   | U | ★
   | m a₀..aₙ
   | match m with
     | φ₀ ⇒ n₀ ..| φₙ ⇒ nₙ
   | λ p₀..pₙ. n
   | {x₀..xₙ : α} → τ
   | {α} → τ
   | ∀ α₀..,αₙ → τ
   | ∀ x₀ : α₀.., xₙ : αₙ → τ
   | (x : α) → τ
   | α → τ *)
type expr =
  | E_u
  | E_src_pos of expr Loc.t
  | E_hole of Symbol.t
  | E_var of Symbol.t
  | E_app of {
      callee : expr;
      spine : (expr * Core.icit) list;
    }
  | E_case of {
      scrutinee : expr;
      cases : (pat * expr) list;
    }
  | E_lam of {
      params : param list;
      expr : expr;
    }
  | E_forall of {
      names : Symbol.t list;
      dom : expr;
      cod : expr;
    }
  | E_pi of {
      name : Symbol.t;
      dom : expr;
      cod : expr;
    }

(* Arg a = {m} | m *)
and arg = Arg of expr * Core.icit

(* Constructor con =
   | x : τ
   | x *)
type constructor =
  | Constructor of {
      name : Symbol.t;
      type_repr : expr option;
    }

(* Toplevel tl =
   | inductive x : α
     | con₀ ..| conₙ
   | inductive x
     | con₀ ..| conₙ
   | x : α
   | x = m *)
type top_level =
  | TL_src_pos of top_level Loc.t
  | TL_inductive of {
      name : Symbol.t;
      type_repr : expr option;
      constructors : constructor list;
    }
  | TL_sig of {
      name : Symbol.t;
      type_repr : expr;
    }
  | TL_clause of {
      name : Symbol.t;
      value : expr;
    }
