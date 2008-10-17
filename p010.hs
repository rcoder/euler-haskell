-- Find the sum of all primes under two million
import Data.List
import Prime

myPrimes = takeWhile (< 2000000) primes

result = sum myPrimes

main = do
  putStrLn $ "value: " ++ (show result)

