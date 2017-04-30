{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.Function (on)

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

--           x   left  right
foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
-- foldTree _ i Leaf = Leaf
-- foldTree acc i = foldMap acc inorder
foldTree acc i Leaf = i
foldTree acc i (Node left x right) = acc x
                                     (foldTree acc i left)
                                     (foldTree acc i right)
-- the i part is wrong, is doing numleaf times
