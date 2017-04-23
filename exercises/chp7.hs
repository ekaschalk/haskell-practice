k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("hi", (1+2))
k3 = k (3, True)

fo :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
fo (a, _, c) (d, _, f) = ((a, d), (c, f))

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
