-- Find the first Fibonacci number with 1000 digits
import Data.List

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

termIndex = findIndex (\n -> (length $ show n) == 1000) fibs
result = maybe 0 (id) termIndex

main = do
 putStrLn $ "value: " ++ (show result)

