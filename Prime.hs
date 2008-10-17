module Prime ( primes ) where

import Euler 

data Wheel = Wheel Integer [Integer]

roll (Wheel sz pts) = [sz * k + r| k <- [0..], r <- pts]

w0 = Wheel 1 [1]

nextSize (Wheel sz pts) scale =
  Wheel (scale * sz) [r' | k <- [0..(scale - 1)], 
                           r <- pts, 
                           let r' = sz * k + r, r' `mod` sz /= 0]

mkWheel divisorList = foldl nextSize w0 divisorList

primes :: [Integer]
primes = fixed ++ large
  where 1:p:candidates = roll $ mkWheel fixed
        fixed          = [2,3,5,7] 
        large          = p: filter isPrime candidates
        isPrime n      = all (not . divides n) $ takeWhile (\i -> i * i <= n) large

