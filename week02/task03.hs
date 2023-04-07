main :: IO()
main = do

    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
    helper d
     | d >= n = True
     | mod n d == 0 = False
     | otherwise = helper (d + 1)


sumPrimeDivs :: Int -> Int
sumPrimeDivs 0 = 0
sumPrimeDivs n = helper 1 
 where 
    helper d 
     | d > n = 0
     | mod n d == 0 && isPrime (d) = d + helper (d + 1)
     | otherwise = helper (d + 1)



