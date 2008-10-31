-- Find the number of triangle words in a wordlist

import qualified Data.Set as S
import Data.Char
import IO
import Euler

triangle n = (n * (n + 1)) `div` 2

trianglesUpto x = takeWhile (<= x) $ map triangle [1..]

-- Max word length is 14 chars, and max alphabetic pos. is 26,
-- so no word can have a value higher than their this
maxWordValue = 14 * 26
triangleSet = S.fromList $ trianglesUpto maxWordValue

isTriangleWord word = (wordAlpha word) `S.member` triangleSet

readWords path = do
  text <- readFile path
  return (words text)

main = do
  allWords <- readWords "words2.txt"
  let triWords = filter isTriangleWord allWords
  let result = length triWords
  putStrLn $ "value: " ++ (show result)
  --putStrLn $ "words: " ++ (show triWords)

