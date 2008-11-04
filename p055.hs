-- Find the count of Lychrel numbers under 10,000

isPalindrome n = (show n) == (reverse $ show n)

reverseDigits :: Integer -> Integer
reverseDigits n = read $ reverse $ show n

reverseAndAdd n = n + (reverseDigits n)

sumSequence n = drop 1 $ iterate reverseAndAdd n

isLychrelNumber n = not (any isPalindrome $ take 50 $ sumSequence n)

lychrelNumbers = filter isLychrelNumber [1..9999]

result = length lychrelNumbers

main = do
  putStrLn $ "value: " ++ (show result)

