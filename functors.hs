-- Functors

-- 16.7 Lifting Exercises

a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap $ fmap (++ "lol")) (Just ["Hi", "Hello"])
c = fmap (* 2) (\x -> x - 2)
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        iostr = fmap show ioi :: IO [Char]
        ioconcat = fmap ("123" ++) iostr :: IO [Char]
        changed = fmap read ioconcat :: IO Integer
    in fmap (* 3) changed
