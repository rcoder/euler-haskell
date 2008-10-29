module Euler ( divides, factors, isPrime, pairs, split, factorial, modexp ) where

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

split :: String -> Char -> [String]
split str delim = pre : (split post delim)
  where pre     = fst chunks
        post    = snd chunks
        chunks  = break (== delim) str

factorial :: Integer -> Integer
factorial n = product [1..n]

modexp :: Integer -> Integer -> Integer -> Integer
modexp _ 0 _ = 1
modexp n p m = if (odd p) then (modProd t n) else t
  where modProd i j = (i * j) `mod` m
        t  = modProd t' t'
        t' = modexp n (p `div` 2) m

