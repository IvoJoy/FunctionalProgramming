main :: IO()
main = do

    print $ countOccurences 121 1 == 2
    print $ countOccurences 222 1 == 0
    print $ countOccurences 100 0 == 2
    print $ countOccurences 0 0 == 1

countOccurences :: Int -> Int -> Int
countOccurences 0 0 = 1
countOccurences x y = helper x y
  where
    helper 0 y = 0
    helper x y
     | mod x 10 == y = 1 + helper (div x 10) y
     | otherwise = helper (div x 10) y