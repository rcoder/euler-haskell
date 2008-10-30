-- Find the sum of all amicable pairs under 10000

import qualified Data.Map as M
import Data.List
import Euler

intRange = [1..10000]

composites = filter (not . null . factors) intRange

sumOfFactors = sum . factors

memoizedSums = M.fromList $ zip composites (map sumOfFactors composites)
fastSumOfFactors n = M.findWithDefault 0 n memoizedSums

amicable i = (not (i == sum')) && ((fastSumOfFactors sum') == i)
  where sum' = fastSumOfFactors i

amicableTerms = filter amicable composites

result = sum amicableTerms

main = do
  putStrLn $ "value: " ++ (show result)
  putStrLn $ "terms: " ++ (show amicableTerms)
