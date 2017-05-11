import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)
import Data.Maybe (maybe, mapMaybe)

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
