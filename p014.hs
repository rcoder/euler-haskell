-- Find the starting number under one million which produces the longest
-- chain of positive integers from a simple function

chain :: Integer -> [Integer]
chain n = n : chain nxt
  where nxt = if even n then n `div` 2
              else 3 * n + 1

isEmpty :: [a] -> Bool
isEmpty lst = case lst of 
  []   -> True
  x:xs -> False

longestListsUpto :: [[Integer]] -> [[Integer]]
longestLists lol = case filter (not . isEmpty) lol of
  []   -> lol
  x:[] -> [x]
  xs   -> longestLists $ map (tail) xs

valRng = [1..1000000]
allChains = map chain valRng

result = longestLists allChains

main = do
  putStrLn $ "value: " ++ (show result)

