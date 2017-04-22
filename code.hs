-- Chapter 5

func1 :: [a] -> a
func1 (x:_) = x

func2 :: Ord a => a -> a -> Bool
func2 x y = if (x > y) then True else False

func3 :: (a, b) -> b
func3 (x, y) = y

i :: a -> a
i = id

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' a b = a

c' :: a -> b -> b
c' a b = b

-- r -> drop/take

co :: (b -> c) -> (a -> b) -> (a -> c)
co bc ab = bc . ab

a :: (a -> c) -> a -> a
a f a = a

a' :: (a -> b) -> a -> b
a' f a = f a

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform xy = (,) (xz $ fst xy) (yz $ snd xy)

xform' :: (X, Y) -> (Z, Z)
xform' (x, y) = (,) (xz x) $ yz y

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g = fst . g . f

-- Chapter 6

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday monthNum)
       (Date weekday' monthNum') =
    weekday == weekday' && monthNum == monthNum'

x = Date Mon 10 == Date Mon 10

--

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x)
       (TisAn y) =
    x == y

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y)
       (Two x' y') =
    x == y && x' == y'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt x == TisAnInt y = x == y
  TisAString x == TisAString y = x == y

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' = x == x' && y == y'

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne x == ThisOne y = x == y
  ThatOne x == ThatOne y = x == y

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x == y
  Goodbye x == Goodbye y = x == y

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x a = f a + fromInteger x
-- arith (\x -> x+1) 2 3.5
