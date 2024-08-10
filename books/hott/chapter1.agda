{-# OPTIONS --cubical #-}

module chapter1 where

data ℕ : Set where
  zero : ℕ
  succ : (pre : ℕ) -> ℕ

