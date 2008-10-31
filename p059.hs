-- Brute-force decrypt a cyphertext using a three-character key

import Data.List
import Data.Bits
import Data.Char
import qualified Data.Set as S

charRng  = ['a'..'z']
keyspace = [a:b:c:[]| a <- charRng, b <- charRng, c <- charRng]

isCandidate text cribSet = any (`S.member` cribSet) (words text)

decryptBits :: String -> String -> Int -> String
decryptBits s k m = map (\i -> chr $ (s `ascAt` i) `xor` (k `ascAt` i)) [0..(m-1)]
  where ascAt t x = ord (t !! x)

decrypt :: String -> String -> String
decrypt text key = case length text of
  0 -> ""
  1 -> decryptBits text key 1
  2 -> decryptBits text key 2
  3 -> decryptBits text key 3
  _ -> (decryptBits text key 3) ++ decrypt (drop 3 text) key

findCandidates cipherText cribList = filter (\(t, k) -> isCandidate t cset) possDecrypts
  where possDecrypts = zip (map (\k -> decrypt cipherText k) keyspace) keyspace
        cset = S.fromList cribList

main = do
  cipherText <- readFile "cipher1_2.txt"
  cribText <- readFile "words2.txt"
  let cribList = words cribText
  let solutions = findCandidates (take 100 cipherText) cribList
  putStr $ concat $ map (\p -> "key: " ++ (snd p) ++ "\nvalue:\n" ++ (fst p) ++ "\n") solutions
  -- putStrLn $ "candidates: " ++ (show solutions)
  -- putStrLn $ "keyspace: " ++ (show $ length keyspace)

