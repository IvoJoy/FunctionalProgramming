main :: IO()
main = do

    print $ eqSumPowDig 100 2
    print $ eqSumPowDig 1000 2
    print $ eqSumPowDig 2000 2
    print $ eqSumPowDig 200 3
    print $ eqSumPowDig 370 3
    print $ eqSumPowDig 400 3
    print $ eqSumPowDig 500 3
    print $ eqSumPowDig 1000 3
    print $ eqSumPowDig 1500 3


    print $ getNthSevenlikeNum 1
    print $ getNthSevenlikeNum 2
    print $ getNthSevenlikeNum 3
    print $ getNthSevenlikeNum 4




sumOfDigitsOnPow :: Int -> Int -> Int
sumOfDigitsOnPow 0 _ = 0
sumOfDigitsOnPow x pow = (mod x 10)^pow + sumOfDigitsOnPow (div x 10) pow

isSpecial :: Int -> Int -> Bool
isSpecial 0 _ = False
isSpecial 1 _ = False
isSpecial n pow
 | n < 2 || pow < 0 = False
 | otherwise = (sumOfDigitsOnPow n pow) == n


eqSumPowDig :: Int -> Int -> Int
eqSumPowDig hMax power
 | hMax < 1 || power < 0 = error "Invalid input!"
 | otherwise = helper hMax power 0
   where
     helper 0 _ sum = sum
     helper hMax power sum
      | isSpecial hMax power = helper (hMax - 1) power (sum + hMax)
      | otherwise = helper (hMax - 1) power sum


makeItBinary :: Int -> Int
makeItBinary 0 = 0
makeItBinary x = (mod x 2) + 10 * makeItBinary (div x 2)

getNthSevenlikeNum :: Int -> Int
getNthSevenlikeNum 0 = 0
getNthSevenlikeNum x
 | x < 0 = error "Invalid input!"
 | otherwise = helper (makeItBinary x) 0
    where
     helper 0 _ = 0
     helper y pow = (mod y 10) * (7^pow) + helper (div y 10) (pow + 1)