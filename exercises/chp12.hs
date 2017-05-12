import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)
import Data.Maybe (maybe, mapMaybe, fromMaybe, fromJust)

-- String exs

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . map (maybe "a" id) . map notThe . words

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

checkNext :: [Maybe String] -> Integer
checkNext (Nothing: (Just x): _) = if isVowel $ x !! 0 then 1 else 0
checkNext _ = 0

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = sum . map checkNext . windows 2 . map notThe . words

countVowels :: String -> Integer
countVowels = sum . map (toInteger . fromEnum . isVowel)

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord x = if vowels > consonants then Just (Word' x) else Nothing
  where vowels = countVowels x
        consonants = toInteger (length x) - vowels

-- Maybe exs

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x == 0 = Just Zero
  | x >= 0 = Just (Succ (fromMaybe Zero $ integerToNat $ x-1))
  | x < 0 = Nothing

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Just a) = f a
mayybee b _ _ = b

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a (Just b) = b
fromMaybe' a _ = a

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _ = []

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x = if any isNothing x then Nothing else Just (catMaybes x)

-- Either exs

accL :: Either a b -> [a] -> [a]
accL (Left x) y = x:y
accL _ y = y

accR :: Either a b -> [b] -> [b]
accR (Right x) y = x:y
accR _ y = y

lefts' :: [Either a b] -> [a]
lefts' = foldr accL []

rights' :: [Either a b] -> [b]
rights' = foldr accR []

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers x = (lefts' x, rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) $ (\x -> Just x) . f

-- Unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x: myIterate f (f x)

-- a is prepended to list, b is next to use in call
-- Nothing signals terminate
-- myUnfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go $ f b
  where go (Just x) = fst x: myUnfoldr f (snd x)
        go Nothing = []

myIterate' :: (a -> a) -> a -> [a]
myIterate' f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = go $ f a
  where go (Just (left, x, right)) = Node (unfold f left) x (unfold f right)
        go Nothing = Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == 0 then Nothing else Just (x-1, n-x, x-1)) n
