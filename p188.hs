-- Problem 188: Find hyper(1777, 1855)

import Euler

hypmod n x m = (map (\i -> modexp i i m) $ repeat n) !! (x-1)

result = hypmod 1777 1855 100000000

main = do
  putStrLn $ "result: " ++ (show result)

