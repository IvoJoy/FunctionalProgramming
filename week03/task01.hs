main :: IO()
main = do

    print $ removeFistOccurrence 110 1 == 10
    print $ removeFistOccurrence 15365 5 == 1536
    print $ removeFistOccurrence 15360 0 == 1536
    print $ removeFistOccurrence 15300 0 == 1530
    print $ removeFistOccurrence 15365 1 == 5365
    print $ removeFistOccurrence 35365 3 == 3565
    print $ removeFistOccurrence 1212 1 == 122
    print $ removeFistOccurrence 1212 2 == 121
    print $ removeFistOccurrence (removeFistOccurrence 1212 1) 1 == 22

removeFistOccurrence :: Int -> Int -> Int
removeFistOccurrence 0 _ = 0
removeFistOccurrence num dig = helper num 0 0 False
 where
    helper 0 result _ _ = result
    helper num result pow flag
     | mod num 10 == dig && not flag = helper (div num 10) result pow True
     | otherwise = helper (div num 10) (result + 10^pow * (mod num 10)) (pow + 1) flag