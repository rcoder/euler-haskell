-- Problem 3: Find the largest prime factor of a composite number
-- (where said number is arbitrary chosen to be 600851475143)

bigNum = 600851475143

factors :: Integer -> [Integer]
factors n = filter (isFactor) [2..limit]
  where isFactor i = n `mod` i == 0
        limit = floor $ sqrt (fromInteger n)

isPrime :: Integer -> Bool
isPrime = null . factors

primeFactors :: Integer -> [Integer]
primeFactors = (filter isPrime) . factors

result = primeFactors bigNum

main = do
  putStrLn $ "prime factors of " ++ (show bigNum) ++ ": " ++ (show result)

