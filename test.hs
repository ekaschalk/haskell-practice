fac n = product [1..n]

absolute x
  | x < 0 = negate x
  | otherwise = x

factorial 0 = 1
factorial n = n * factorial (n-1)

mylength :: [a] -> Int
mylength []     = 0
mylength (x:xs) = 1 + mylength xs
