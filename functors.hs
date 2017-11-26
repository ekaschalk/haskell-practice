-- Functors

import Test.QuickCheck
import Test.QuickCheck.Function

-- 16.7 Lifting Exercises

a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap $ fmap (++ "lol")) (Just ["Hi", "Hello"])
c = fmap (* 2) (\x -> x - 2)
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        iostr = fmap show ioi :: IO [Char]
        ioconcat = fmap ("123" ++) iostr :: IO [Char]
        changed = fmap read ioconcat :: IO Integer
    in fmap (* 3) changed

-- 16.10 Intermission
--- given example

functorCompose' :: (Eq (f c), Functor f) =>
  f a -> Fun a b -> Fun b c -> Bool

functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

--- Ex1

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

type IDFC = Identity Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

--- Ex2

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

--- Ex3

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

type StringToInt = Fun String Int
type StringToString = Fun String String
type TwoFC = Two Int String -> StringToString -> StringToInt -> Bool
type TwoFC' = Two Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

--- Ex4

main :: IO ()
main = do
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorCompose' :: IDFC)
  quickCheck (functorCompose' :: PairFC)
  quickCheck (functorCompose' :: TwoFC)
  quickCheck (functorCompose' :: TwoFC')
