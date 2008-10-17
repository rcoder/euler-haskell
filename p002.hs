-- Problem 2: Find the summ of all the even-valued terms in the
-- Fibonacci sequence which do not exceed four million

fib = 1 : 2 : zipWith (+) fib (tail fib)

evnFib = filter (even) fib

terms = takeWhile (< 4000000) evnFib
total = sum terms

main = do
  putStrLn $ "total: " ++ (show total)

