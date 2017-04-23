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
joinStr = map (\x -> if x == ' ' then '\n' else x)
-- joinStr "hello doggy run away" == "hello\ndoggy\nrun\naway"

myzip :: [a] -> [b] -> [(a, b)]
myzip a b = go a b []
  where go (a:as) (b:bs) zipped
          | null as || null bs = reverse items
          | otherwise = go as bs items
          where items = (a, b):zipped
-- myzip [1, 2, 3] [2, 3, 4, 5] == [(1,2),(2,3),(3,4)]

myzipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipwith f x y = uncurry f <$> zip x y
-- myzipwith (\x y -> x+y+1) [1, 2, 3] [2, 3, 4, 5] == [4,6,8]

myzip' :: [a] -> [b] -> [(a, b)] -- in terms of zipwith
myzip' = myzipwith (,)
-- myzipwith (,) [1, 2, 3] [2, 3, 4, 5] == [(1,2),(2,3),(3,4)]

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (> 0) . length . filter f
-- myAny (\x -> x > 2) [1, 2] == False
-- myAny (\x -> x > 2) [1, 2, 3] == True

myElem :: Eq a => a -> [a] -> Bool
myElem item = any (== item)
-- myElem 1 [1..10] == True
-- myElem 1 [2..10] == False

squish :: [[a]] -> [a]
squish (x:[]) = x
squish (x:xs) = x ++ squish xs
-- squish [[1..4], [1..3]] == [1,2,3,4,1,2,3]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f
-- squishMap (\x -> [x..x+2]) [1..4] == [1,2,3,2,3,4,3,4,5,4,5,6]

squish' :: [[a]] -> [a]  -- in terms of squishmap
squish' = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp x = go cmp x
  where
    go cmp [] = undefined
    go cmp (x:[]) = x
    go cmp (x:y:[]) = biggest x y
    go cmp (x:y:xs) = go cmp $ biggest x y:xs
    biggest x y = if cmp x y == GT then x else y
-- myMaximumBy (\_ _ -> LT) [1..10] == 1
-- myMaximumBy (\_ _ -> GT) [1..10] == 1
