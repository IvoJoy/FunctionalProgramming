main :: IO()
main = do

    print $ sumOfDigits 267
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990


sumOfDigits :: Int -> Int
sumOfDigits 0 = 0
sumOfDigits x = (mod x 10) + sumOfDigits (div x 10)

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k
 | start < 0 || finish < 0 || k <= 0 = error "Wrong input"
 | otherwise = helper (min start finish) (max start finish)
  where
    helper start finish
     | start == finish = 0
     | mod (sumOfDigits start) k == 0 = start + helper (start + 1) finish
     | otherwise = helper (start + 1) finish
