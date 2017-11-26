-- Applicative

import Data.List (elemIndex)
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Function


-- 17.5 exercises pg 788

added :: Maybe Integer
added = (+3) <$> (lookup 2 $ zip [1, 2] [3, 4])

y :: Maybe Int
y = lookup 2 $ zip [1, 2] [3, 4]

x :: Maybe Int
x = elemIndex 3 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y
-- maxed = liftA2 max' x y

as = [1..3]
bs = [4..6]

a :: Maybe Integer
a = lookup 3 $ zip as bs

b :: Maybe Integer
b = lookup 2 $ zip as bs

summed :: Maybe Integer
summed = sum <$> liftA2 (,) a b
-- summed = fmap sum $ (,) <$> a <*> b
