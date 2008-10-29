-- Find the sum of all primes less than 40000000 which generate a chain
-- of values for Euler's totient function of length 25

totient :: Integer -> Integer
totient n = length $ filter ((== 1) . (gcd n)) [1..n]


