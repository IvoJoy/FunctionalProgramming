main :: IO()
main = do

    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789

rev :: Int -> Int
rev x
 | x < 0 = error "x was negative"
 | otherwise = helper x 0
 where
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (10 * result + mod leftover 10)