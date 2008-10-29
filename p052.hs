-- Find the smallest positive integer x such that 2x..6x contain the same
-- digits, disregarding order

import Data.List

digitsOf = sort . show

sameDigits i j = (digitsOf i) == (digitsOf j)
sameDigitsInList lst = all (sameDigits (head lst)) lst

twoToSixTimes n = map (* n) [1..6]

terms = map twoToSixTimes [1..]
validTerms = filter sameDigitsInList terms
result = head validTerms

main = do
  putStrLn $ "value: " ++ (show result)

