module FastPower where

toBinaryHelper :: Integral a => a -> [a]
toBinaryHelper 0 = [0]
toBinaryHelper x = mod x 2 : toBinaryHelper (div x 2)

toBinary :: Integral a => a -> [a]
toBinary x = tail $ reverse $ toBinaryHelper x

repeatedSquare :: (Integral a) => a -> a -> Int -> [a]
repeatedSquare a m n = take n (iterate (\x -> x * x `mod` m) a)

mask :: (Eq a, Num a) => [a] -> [b] -> [b]
mask xs ys = map snd $ filter (\x -> fst x == 1) (zip xs ys)

fastPower :: (Integral a1, Integral a2) => a1 -> a2 -> a1 -> a1
fastPower a b m = product (mask binb (repeatedSquare a m (length binb))) `mod` 100
  where
    binb = toBinary b

-- convert :: Read a => String -> [a]
-- convert = map read . words

-- main :: IO ()
-- main = do
--   line <- getLine
--   let [a, b, m] = convert line :: [Int] in print (fastPower a b m)