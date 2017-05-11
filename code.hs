import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)
import Data.Maybe (maybe, mapMaybe, fromMaybe)

-- chp12

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
