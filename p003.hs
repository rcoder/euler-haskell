-- Problem 3: Find the largest prime factor of a composite number
-- (where said number is arbitrary chosen to be 600851475143)

import Euler

bigNum = 600851475143

primeFactors :: Integer -> [Integer]
primeFactors = (filter isPrime) . factors

result = primeFactors bigNum

main = do
  putStrLn $ "prime factors of " ++ (show bigNum) ++ ": " ++ (show result)

