main :: IO()
main = do

    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134

rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)

removeD :: Int -> Int -> Int
removeD d n = helper (rev n) 0
 where
    helper 0 result = result
    helper num result
     | mod num 10 == d = helper (div num 10) result
     | otherwise = helper (div num 10) (10 * result + mod num 10)