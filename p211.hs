-- Find the sum of all numbers such that the sum of the squares of its
-- divisors is itself a square

intSqrt :: Integer -> Integer
intSqrt n = floor $ sqrt (fromInteger n)

factors :: Integer -> [Integer]
factors n = filter ((== 0) . (n `mod`)) [1..(intSqrt n)]

isSquare :: Integer -> Bool
isSquare n = sq*sq == n
  where sq = intSqrt n

sumSod :: Integer -> Integer
sumSod n = sum $ map (\x -> x*x) (factors n)

terms = filter (isSquare . sumSod) [1..64000000]
result = sum terms

main = do
  putStrLn $ "value: " ++ (show result)

