module Nat where

  data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)

  instance Num Nat where
    (+) a Zero = a
    (+) Zero a = a
    (+) (Succ a) (Succ b) = Succ (Succ(a + b))
    (*) a Zero = Zero
    (*) a (Succ b) = (a * b) + a

    abs a = a
    signum Zero = Zero
    signum _ = Succ Zero

    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n - 1))

    negate _ = error "negate is undefined for Nat"

  beside :: Nat -> Nat -> Bool
  beside a b = a == (Succ b) || b == (Succ a)

  beside2 :: Nat -> Nat -> Bool
  beside2 a b = a == (Succ (Succ b )) || b == (Succ (Succ a))

  pow :: Nat -> Nat -> Nat
  pow a (Succ b) = a * (pow a b)
  pow Zero x = Zero
  pow x Zero = 1
