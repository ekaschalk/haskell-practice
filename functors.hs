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

main :: IO ()
main = do
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorCompose' :: IDFC)
