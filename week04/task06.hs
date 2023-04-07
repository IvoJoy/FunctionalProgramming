import Data.Char
import Data.List

main :: IO()
main = do

    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0]

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [ x | x <- [1 ..], isPrime x, elem d $ map digitToInt $ show x] -- yes, the $ sign says to do the left function AFTER the right one is done