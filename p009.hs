-- Find the product of the Pythagorean triplet where a + b + c == 1000

import Data.List

pairs :: [a] -> [(a,a)]
pairs []      = []
pairs (x:xs)  = [(x,x)] ++ (map (\i -> (x,i)) xs) ++ pairs xs

valRng = [1..999]
squares = map (\i -> i*i) valRng

sqrtMap = zip squares valRng

lhs p = a*a + b*b
  where a = fst p
        b = snd p

sumsOfSquares = filter (\p -> elem (lhs p) squares) (pairs valRng)

triples = map (\p -> ((fst p), (snd p), (maybe 0 (id) (lookup (lhs p) sqrtMap)))) sumsOfSquares

resultTuple = head $ filter (\(a,b,c) -> a + b + c == 1000) triples

prod3 (i,j,k) = i*j*k

result = prod3 resultTuple

main = do
  putStrLn $ "value: " ++ (show result)

