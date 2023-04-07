main :: IO()
main = do

    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6

sumDigitsIter :: Int -> Int
sumDigitsIter 0 = 0
sumDigitsIter x = helper x
 where
     helper 0 = 0
     helper x = (mod x 10) + helper (div x 10)