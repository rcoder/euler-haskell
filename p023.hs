-- Find the sum of all positive integers which cannot be written
-- as the sum of two abundant numbers

import qualified Data.Set as S
import Data.List
import Euler

sumOfFactors = sum . factors
isAbundant n = (sumOfFactors n) > n

searchSpace = [12..28123]

abundants = 12:filter isAbundant searchSpace
  where isAbundant n = (sumOfFactors n) > n

isSumOfAbundants n = any (\i -> (n - i) `S.member` known) searchSpace
  where known = S.fromList abundants

-- isSumOfAbundants n = any (\i -> (n - i) `S.member` abundants) searchSpace

--allNotSums = S.filter (not . isSumOfAbundants) candidates
--  where candidates = S.difference (S.fromList searchSpace) abundants

-- result = sum (S.toList allNotSums)

main = do
  putStrLn $ "value: " ++ (show (length abundants))

