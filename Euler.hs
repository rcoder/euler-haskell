module Euler ( divides, factors, isPrime, pairs ) where

divides :: Integer -> Integer -> Bool
divides i j = i `mod` j == 0

factors :: Integer -> [Integer]
factors n = filter (isFactor) [2..limit]
  where isFactor i = divides n i 
        limit = floor $ sqrt (fromInteger n)

isPrime :: Integer -> Bool
isPrime = null . factors

pairs :: [a] -> [(a,a)]
pairs []      = []
pairs (x:xs)  = [(x,x)] ++ (map (\i -> (x,i)) xs) ++ pairs xs

