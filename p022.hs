-- Find the named score for a large list of common names

import IO
import Data.List
import Euler ( wordAlpha )

readNames path = do
  nameData <- readFile path
  let nameList = words nameData
  return (nameList)

ordNameAlist :: [String] -> [(Int, String)]
ordNameAlist names = zip [1..] (sort names)

nameScores :: [(Int, String)] -> [Int]
nameScores nlst = map scoreOne nlst
  where scoreOne (pos, name) = pos * (wordAlpha name)

main = do
  names <- readNames "names2.txt"
  let namesByIndex = ordNameAlist names
  let totalScore = sum $ nameScores namesByIndex
  putStrLn $ "value: " ++ (show totalScore)
  -- putStrLn $ "names: " ++ (show namesByIndex)

