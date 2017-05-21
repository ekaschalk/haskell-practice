-- chp15

import Test.QuickCheck
import Data.Monoid
-- import Data.Monoid hiding ((<>))
-- import Data.Semigroup


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only $ a <> b
  mappend (Only a) Nada = Only $ a <> mempty a
  mappend _ (Only b) = Only $ b <> mempty b
  mappend _ _ = Nada

-- Only (Sum 1) <> Only (Sum 2)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

type S = String
type B = Bool

newtype First' a =
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) b =  First' (Only a)
  mappend _ (First' (Only b)) =  First' (Only b)
  mappend _ _ = First' Nada

-- First' (Only 1) `mappend` First' Nada
-- First' Nada `mappend` First' (Only 1)
-- First' (Only 2) `mappend` First' (Only 1)
-- First' Nada `mappend` First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend a b = mappend a b

-- instance Arbitrary (First' a) where
instance Arbitrary (Optional a) where
  arbitrary = do
    Only a <- arbitrary
    return $ Only a

instance Arbitrary (First' a) where
  arbitrary = do
    First' a <- arbitrary
    return $ First' a


type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool


main :: IO ()
main = do
  -- quickCheck (monoidAssoc :: S -> S -> S -> B)
  quickCheck (monoidAssoc :: FirstMappend)
  -- quickCheck (monoidLe)
