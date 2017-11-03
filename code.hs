{-# LANGUAGE PackageImports #-}

import Test.QuickCheck
import "base" Data.Semigroup

-- * Semigroups
-- ** Ex1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- ** Ex2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  (<>) (Identity a) _ = Identity a

instance Arbitrary (Identity a) where
  arbitrary = do
    Identity a <- arbitrary
    return $ Identity a

-- ** Ex3

data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
  (<>) (Two a b) _ = Two a b

-- Ex4/5 same as 2/3 in that we simply return the LHS (or RHS)
-- (1 2) (3 4) (5 6) (7 8) => always (1 2) or (7 8) regardless of parens

-- ** Ex6

newtype BoolConj =
  BoolConj Bool

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _ = BoolConj False

-- Ex7 same but disjunction

-- ** Ex8

data Or a b =
    Fst a
  | Snd b

instance Semigroup (Or a b) where
  (<>) (Snd a) _ = Snd a
  (<>) _ (Snd b) = Snd b
  (<>) (Fst a) _ = Fst a

-- ** Ex9

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ f . g
  -- f : a->b
  -- g : a->b
  -- need to return f' : a->b
  -- where both f and g are applied
  -- do i make a->a->a or b->b->b somehow?

  -- const is a->b->a

-- ** Quickcheck

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type SemigroupMappend = Trivial -> Trivial -> Trivial -> Bool
type IDSemigroupMappend =
  Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: IDSemigroupMappend)
