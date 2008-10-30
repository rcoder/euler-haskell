-- Find the number of Sundays which fell on the first of the month
-- from 1-Jan-1901 to 31-Dec-2000

import Data.Time.Calendar

startDay = fromGregorian 1901 1 1
endDay   = fromGregorian 2000 12 31
dateRng  = [startDay..endDay]

firstOfMonth :: Day -> Bool
firstOfMonth d = case toGregorian d of
  (_, _, 1) -> True
  _ -> False

-- We represent day of the week as integers, where Monday is 0
-- (This depends on the knowledge that 1-Jan-1900 was a Monday)

dayOfWeek :: Day -> Integer
dayOfWeek d = (diffDays d centuryRef) `mod` 7
  where centuryRef = fromGregorian 1900 1 1

mondaysOnFirst = filter (\d -> firstOfMonth d && (dayOfWeek d) == 6) dateRng

result = length mondaysOnFirst

main = do
  putStrLn $ "value: " ++ (show result)

