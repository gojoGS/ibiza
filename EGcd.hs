module EGcd where

euclidianGcd :: Integral t => t -> t -> t
euclidianGcd a b = if b == 0 then abs a else euclidianGcd b (a `mod` b)

solveModular :: Integral a => a -> a -> a
solveModular a m = if i < 0 then i + m else i
  where
    (i, _, g) = extendedEuclidianGcd a m

extendedEuclidianGcd :: Integral b => b -> b -> (b, b, b)
extendedEuclidianGcd a 0 = (1, 0, a)
extendedEuclidianGcd a b = (t, s - q * t, g)
  where
    (q, r) = a `quotRem` b
    (s, t, g) = extendedEuclidianGcd b r

-- convert :: Read a => String -> [a]
-- convert = map read . words

-- main :: IO ()
-- main = do
--   line <- getLine
--   let [a, b] = convert line :: [Int] in print (euclidianGcd a b)