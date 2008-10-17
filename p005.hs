-- What is the smallest number divisible by all numbers from 1 to 20?

result = foldr (lcm) 1 [1..20]

main = do
  putStrLn $ "value: " ++ (show result)

