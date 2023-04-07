main :: IO()
main = do

    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

removeFistOccurrence :: Int -> Int -> Int
removeFistOccurrence 0 _ = 0
removeFistOccurrence num dig = helper num 0 0 False
 where
    helper 0 result _ _ = result
    helper num result pow flag
     | mod num 10 == dig && not flag = helper (div num 10) result pow True
     | otherwise = helper (div num 10) (result + 10^pow * (mod num 10)) (pow+1) flag


biggestNum :: Int -> Int -> Int
biggestNum 0 dig = dig
biggestNum num dig
 | dig > mod num 10 = biggestNum (div num 10) (mod num 10)
 | otherwise = biggestNum (div num 10) dig


sortN :: Int -> Int
sortN num = helper num 0
 where
    helper 0 pow = 0
    helper num pow = ((biggestNum (div num 10) (mod num 10)) * (10^pow)) + helper (removeFistOccurrence num (biggestNum (div num 10) (mod num 10))) (pow + 1)