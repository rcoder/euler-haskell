-- Find the sum of the digits in 2**1000

import Data.List
import Data.Char

powersOf2 = iterate (* 2) 2

bigNum = head $ reverse $ take 1000 powersOf2
digits = map digitToInt (show bigNum)
result = sum digits 

main = do
  putStrLn $ "value: " ++ (show result)

