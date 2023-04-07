main :: IO()
main = do

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6

    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6


mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs


mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
 | null xs = 0
 | otherwise = head xs + (mySumRecNonPM $ tail xs)

mySumFunc :: [Int] -> Int
mySumFunc = sum