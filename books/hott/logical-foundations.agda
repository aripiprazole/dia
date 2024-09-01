{-# OPTIONS --cubical #-}

module logical-foundations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)

\
-- 10 ≤ 20
data _≤_ : ℕ -> ℕ -> Set where
  z≤n : ∀ {m : ℕ} {n : suc m}
        -----------
        -> zero ≤ n

  s≤s : ∀ {m n}
        -> m ≤ n
        ----------------
        -> suc m ≤ suc n

∀ 