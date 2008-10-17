-- Find the difference between the sum of the squares of the first 
-- hundred natural numbers and the square of the sum

range = [1..100]

sumOfSquares = sum $ map (\i -> i*i) range
squareOfSum = s*s where s = sum range

result = squareOfSum - sumOfSquares

main = do
  putStrLn $ "result: " ++ (show result)

