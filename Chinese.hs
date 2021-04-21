module Chinese where

import Data.List (subsequences)
import EGcd (euclidianGcd, solveModular)

combinations :: [a] -> [[a]]
combinations xs = filter (\x -> length x == 2) $ subsequences xs

isRelativePrimes :: Integral a => [a] -> Bool
isRelativePrimes xs = all (\(x : y : _) -> euclidianGcd x y == 1) (combinations xs)

getM :: (Foldable t, Num a) => t a -> a
getM = product

getMis :: Integral b => b -> [b] -> [b]
getMis bigM = map (bigM `div`)

getYis :: Integral c => [c] -> [c] -> [c]
getYis = zipWith solveModular

chineseRemainderTheorem :: Integral b => [(b, b)] -> b
chineseRemainderTheorem xys = sum (zipWith3 (\a y m -> a * y * m) as (getYis bigMs ms) bigMs) `mod` getM ms
  where
    as = map fst xys
    ms = map snd xys
    bigMs = getMis (getM ms) ms