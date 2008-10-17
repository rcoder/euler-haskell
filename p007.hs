-- Find the 10001 prime

primes = 2:filter isPrime [3,5..]
  where isPrime n = all (not . divides n) $ takeWhile (\i -> i*i <= n) primes
        divides n p = n `mod` p == 0

result = primes !! 10000

main = do
  putStrLn $ "value: " ++ (show result)

