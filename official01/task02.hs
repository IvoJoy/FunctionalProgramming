main :: IO()
main = do

    print $ getNthSevenlikeNum 1
    print $ getNthSevenlikeNum 2
    print $ getNthSevenlikeNum 3
    print $ getNthSevenlikeNum 4



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