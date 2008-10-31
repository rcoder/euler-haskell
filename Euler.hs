module Euler ( divides, 
               factors, 
               factorsUptoSqrt, 
               isPrime, 
               pairs, 
               split, 
               factorial, 
               modexp,
               triangle,
               charAlpha,
               wordAlpha ) where

import Data.Char

divides :: Integer -> Integer -> Bool
divides i j = i `mod` j == 0

factorsUpto :: Integer -> Integer -> [Integer]
factorsUpto n i = filter (n `divides`) [1..i]

factorsUptoSqrt :: Integer -> [Integer]
factorsUptoSqrt n = factorsUpto n (floor $ sqrt (fromInteger n))

factors :: Integer -> [Integer]
factors n = factorsUpto n (n-1)

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

triangle n = (n * (n + 1)) `div` 2

charAlpha c = (ord c - ord 'A') + 1
wordAlpha w = sum $ map charAlpha w

