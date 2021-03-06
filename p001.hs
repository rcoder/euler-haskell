-- Problem 1: Add all the natural numbers below one thousand that are
-- multiples of 3 or 5

import Euler

mult3or5 :: Integer -> Bool
mult3or5 i = (divides i 3) || (divides i 5)

values = filter (mult3or5) [1..999]
total = sum values

main = do
  putStrLn $ "total: " ++ (show total)

