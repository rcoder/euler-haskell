-- Find the last ten digits of the series 1**1 + 2**2 + ... + 1000**1000

import Euler ( modexp )

terms = zip [1..1000] [1..1000]
sumOfTerms = sum $ map (\p -> modexp (fst p) (snd p) 10000000000) terms

result = drop ((length val) - 10) val
  where val = show sumOfTerms

main = do
  putStrLn $ "value: " ++ result
