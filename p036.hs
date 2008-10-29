-- Find the sum of all numbers under one million which are palindroms 
-- in both base 10 and base 2

import Data.Char
import Data.List

isPalindrome n = (show n) == (reverse $ show n)

convertToBinary :: Int -> String
convertToBinary n = case n of
  0 -> "0"
  1 -> "1"
  _ -> (intToDigit $ n `mod` 2) : convertToBinary (n `div` 2)

allYourBase n = isPalindrome n && isPalindrome (convertToBinary n)

terms = filter allYourBase [1..1000000]
result = sum terms

main = do
  putStrLn $ "value: " ++ (show result)

