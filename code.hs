{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.Char
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord

-- import Data.Foldable

-- 11.5

data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price =
  Price Integer
  deriving (Eq, Show)

data Size =
  Size Integer
  deriving (Eq, Show)

data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
  PapuAir
  | CatapultsAir
  | UnitedAir
  deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar (Car x y) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane x y) = True
isPlane _           = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x y) = x
getManu _         = undefined

-- 11.6
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 42

instance TooMany (Int, String) where
  tooMany (m, n) = tooMany m

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany = tooMany . uncurry (+)

-- tooMany 100 -> ambiguous type variable exception
-- tooMany (Goats 100) == True
-- tooMany (100 :: Int) == True
-- tooMany (3 :: Int, "hi") == False
-- tooMany (40 :: Int, 0 :: Int) == False
-- tooMany (40, 0) -> throws exception

-- 11.9
-- Record syntax

-- data Person = MkPerson String Int deriving (Eq, Show)

data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)

data JamJars =
  Jam { fruit :: Fruit
      , jars :: Int }
      deriving (Eq, Ord, Show)

countJars :: [JamJars] -> Int
countJars = sum . map jars

largestJar :: [JamJars] -> JamJars
largestJar = maximumBy (compare `on` jars)

sortByJams :: [JamJars] -> [JamJars]
sortByJams = sortBy (compare `on` fruit)

groupJams :: [JamJars] -> [[JamJars]]
groupJams = groupBy ((==) `on` fruit)


-- 11.15

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

-- mapTree (+1) (Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf))
-- == Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

-- foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
-- foldTree acc i Leaf = i
-- foldTree acc i (Node left x right) = acc x
--                                      (foldTree acc i left)
--                                      (foldTree acc i right)

-- foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
-- foldTree acc i tree = go acc tree
--   where go _ Leaf = undefined
--         go acc (Node left x right) = acc x (go acc left) (go acc right)

-- foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
-- foldTree acc i = foldr acc i . inorder

-- mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
-- mapTree' f = foldTree (f . (:))

isSub :: (Eq a) => [a] -> [a] -> Bool
isSub (x:[]) (y:[]) = x == y
isSub (x:xs) (y:ys) = if x == y then isSub xs ys else isSub (x:xs) ys
isSub _ (y:_) = True
isSub _ _ = False

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x:xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\(x:xs) -> (x:xs, toUpper x:xs)) . words

capitalizeParagraph :: String -> String
capitalizeParagraph [] = ""
capitalizeParagraph xs = capitalizeWord $ take sentence xs ++
                         (capitalizeParagraph $ drop sentence xs)
  where sentence = adjustIndex $ Data.List.findIndex (== '.') xs
        adjustIndex (Just x) = 2 + x
        adjustIndex _ = length xs

-- Phone exercise

-- data Phone = Phone (Map Char String) deriving Show

-- phone :: Phone
-- phone = Phone (Map.fromList [('1', ""), ('2', "ABC"), ('3', "DEF"),
--                              ('4', "GHI"), ('5', "JKL"), ('6', "MNO"),
--                              ('7', "PQRS"), ('8', "TUV"), ('9', "WXYZ"),
--                              ('*', "^"), ('0', "+_"), ('#', ".,")])

-- data Phone = Map Char String

phone :: Map Char String
phone = Map.fromList [('1', ""), ('2', "ABC"), ('3', "DEF"),
                       ('4', "GHI"), ('5', "JKL"), ('6', "MNO"),
                       ('7', "PQRS"), ('8', "TUV"), ('9', "WXYZ"),
                       ('*', "^"), ('0', "+_"), ('#', ".,")]

type Digit = Char
type Presses = Int

pressed :: Map Char String -> Char -> (Digit, Presses)
pressed p digit = (k, v)
  where pair = Map.elemAt 0 $ Map.filter (elem digit) p
        k = fst pair
        v = (+ 1) $ Data.Maybe.fromJust $ findIndex (== digit) $ snd pair

-- cellPhonesDead :: Map Char String -> String -> [(Digit, Presses)]
-- cellPhonesDead p = map (pressed p)

-- cellPhonesDead phone "MNOPQERS"
-- [('6',1),('6',2),('6',3),('7',1),('7',2),('3',2),('7',3),('7',4)]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = Map.fold (+) 0 . Map.fromListWith (+)

reverseTaps :: Map Char String -> Char -> [(Digit, Presses)]
reverseTaps p digit = res
  where pair = Map.elemAt 0 $ Map.filter (elem . toUpper $ digit) p
        k = fst pair
        v = (+ 1) $ Data.Maybe.fromJust $ findIndex (== toUpper digit) $ snd pair
        fstPair = (k, v)
        res = if isUpper digit then [fstPair, ('*', 1)] else [fstPair]

cellPhonesDead :: Map Char String -> String -> [(Digit, Presses)]
cellPhonesDead p = concat . map (reverseTaps p)

popularestDigit :: Map Char String -> String -> Char
popularestDigit p = fst . maximumBy (comparing snd) .
                    Map.toList . Map.fromListWith (+) . cellPhonesDead p

coolestChar :: String -> Char
coolestChar = flip (!!) 0 . maximumBy (comparing length) . groupBy (==) . sort

-- Above should work as is for coolestWord as well
