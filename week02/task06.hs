main :: IO()
main = do

    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 


sumDigitsIter :: Int -> Int
sumDigitsIter 0 = 0
sumDigitsIter x = helper x
 where
     helper 0 = 0
     helper x = (mod x 10) + helper (div x 10)

isInteresting :: Int -> Bool
isInteresting x = mod x (sumDigitsIter x) == 0
