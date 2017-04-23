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
