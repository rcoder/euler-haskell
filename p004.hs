-- Find the largest palindromic number that is the product of two
-- three-digit numbers.

import Data.List

splitInCenter :: String -> (String, String)
splitInCenter s = (take preLen s, drop sufLen s)
  where preLen = (length s) `div` 2
        sufLen = if (even $ length s) then preLen else preLen + 1

isPalindrome :: Integer -> Bool
isPalindrome n = fstPart == reverse (sndPart)
  where fstPart = fst pieces
        sndPart = snd pieces
        pieces = splitInCenter (show n)

threeDigitNumbers = [100..999]
desc3DigitNumbers = reverse threeDigitNumbers

pairs :: a -> [a] -> [(a,a)]
pairs i xs = [(i,i)] ++ map (\xi -> (i,xi)) xs

operands = concat $ map (\e -> pairs e desc3DigitNumbers) desc3DigitNumbers

multT :: (Integer, Integer) -> Integer
multT t = (fst t) * (snd t)

allPalindromes = map (multT) $ filter (\p -> isPalindrome (multT p)) operands
result = maximum allPalindromes

main = do
 putStrLn $ "value: " ++ (show result)
-- putStrLn $ "operands: " ++ (show operands)

