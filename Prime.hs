module Prime ( primes ) where

import Euler 

primes :: [Integer]

primes = 2:filter isPrime [3,5..]
  where isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes

