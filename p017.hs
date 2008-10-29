-- Find the number of letters in the English conversion of all the 
-- numbers between 1 and 1000

numberParts n = (thousands, hundreds, tens, ones)
  where thousands = n `div` 1000
        hundreds  = (n `mod` 1000) `div` 100
        tens      = (n `mod` 100) `div` 10
        ones      = n `mod` 10

numberToWords n = thousandStr ++ hundredStr ++ tenStr ++ oneStr
  where thousandStr = if thousands > 0 then
                        digitWord thousands ++ "thousand"
                      else ""
        hundredStr  = case (hundreds, tens, ones) of
                        (0, _, _) -> ""
                        (_, 0, 0) -> digitWord hundreds ++ "hundred"
                        (_, _, _) -> digitWord hundreds ++ "hundred" ++ "and"
        tenStr      = tensWord tens
        oneStr      = if ones > 0 || tens == 1 then
                        case tens of
                          1 -> digitWord (10 + ones)
                          _ -> digitWord ones
                      else ""
        (thousands, hundreds, tens, ones) = numberParts n

tensWord n = case n of
  2 -> "twenty"
  3 -> "thirty"
  4 -> "forty"
  5 -> "fifty"
  6 -> "sixty"
  7 -> "seventy"
  8 -> "eighty"
  9 -> "ninety"
  _ -> ""

digitWord n = case n of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "fix"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  10 -> "ten"
  11 -> "eleven"
  12 -> "twelve"
  13 -> "thirteen"
  14 -> "fourteen"
  15 -> "fifteen"
  16 -> "sixteen"
  17 -> "seventeen"
  18 -> "eighteen"
  19 -> "nineteen"
  _ -> ""

allWords = map numberToWords [1..1000]
result = length $ concat allWords

main = do
  putStrLn $ "value: " ++ (show result)
  putStrLn $ "allWords: " ++ (unlines allWords)
