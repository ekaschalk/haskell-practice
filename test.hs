fac n = product [1..n]

absolute x
  | x < 0 = negate x
  | otherwise = x

test x = 3:4

main =
  do putStrLn (getLine)
