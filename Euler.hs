module Euler ( divisible, factors, isPrime, pairs, primes ) where

divisible :: Integer -> Integer -> Bool
divisible i j = i `mod` j == 0

factors :: Integer -> [Integer]
factors n = filter (isFactor) [2..limit]
  where isFactor i = divisible n i 
        limit = floor $ sqrt (fromInteger n)

isPrime :: Integer -> Bool
isPrime = null . factors

pairs :: [a] -> [(a,a)]
pairs []      = []
pairs (x:xs)  = [(x,x)] ++ (map (\i -> (x,i)) xs) ++ pairs xs

primes = 2:filter isPrime [3,5..]
  where isPrime n = all (not . divisible n) $ takeWhile (\i -> i*i <= n) primes

