-- Find the largest palindromic number that is the product of two
-- three-digit numbers.

import Data.List

isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == reverse (show n)

threeDigitNumbers = [100..999]
desc3DigitNumbers = reverse threeDigitNumbers

pairs :: [a] -> [(a,a)]
pairs []      = []
pairs (x:xs)  = [(x,x)] ++ (map (\i -> (x,i)) xs) ++ pairs xs

operands = pairs desc3DigitNumbers

multT :: (Integer, Integer) -> Integer
multT t = (fst t) * (snd t)

products = map (multT) operands

palindromes = filter (isPalindrome) products
result = maximum palindromes 

main = do
 putStrLn $ "value: " ++ (show result)
-- putStrLn $ "num. palindromes: " ++ (show $ length allPalindromes)

