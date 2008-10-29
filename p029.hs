-- Find the number of terms in the sequence a^b for some range of a, b

import Data.List

allTerms = [a ** b| a <- [2..100], b <- [2..100]]
uniqueTerms = nub $ sort allTerms

result = length uniqueTerms

main = do
  putStrLn $ "value: " ++ (show result)

