{-# LANGUAGE FlexibleInstances #-}



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
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane x y) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x y) = x
getManu _ = undefined

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
