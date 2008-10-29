-- Problem 20: Find the sum of the digits in 100!

import Data.Char
import Euler

factStr = show (factorial 100)
digits = map digitToInt factStr

result = sum digits

main = do
  putStrLn $ "value: " ++ (show result)

