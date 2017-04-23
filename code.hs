-- chp9
myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo x y = take (fromEnum y) $ iterate succ x
-- myEnumFromTo 1 5 == [1..5]

splitStr :: String -> [String]
splitStr sentence = split1 sentence []
  where split1 sentence words
          | null sentence = reverse words
          | otherwise = split1 (dropWord sentence) $ getWord sentence : words
          where getWord = takeWhile (/= ' ')
                dropWord = drop 1 . dropWhile (/= ' ')
-- splitStr "hello doggy run away" == ["hello","doggy","run","away"]

joinStr :: String -> String
joinStr sentence = map addNewLine sentence where
  addNewLine x
    | x == ' ' = '\n'
    | otherwise = x
-- joinStr "hello doggy run away" == "hello\ndoggy\nrun\naway"
