-- Summing a List
mysum :: [Int] -> Int
mysum xs
  | xs == [] = 0
  | otherwise = head xs + mysum (tail xs)

-- Pattern matching version
mysum2 :: [Int] -> Int
mysum2 [] = 0
mysum2 xs = head xs + mysum2 (tail xs)

-- Using cons in pattern matching
mysum3 :: [Int] -> Int
mysum3 [] = 0
mysum3 (x:xs) = x + mysum3 xs

-- xxx
