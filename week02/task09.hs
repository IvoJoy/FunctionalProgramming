main :: IO()
main = do

    print $ everyOther 12 == 1
    print $ everyOther 852369
    print $ everyOther 1714 == 11
    print $ everyOther 12345
    print $ everyOther 891 
    print $ everyOther 123 
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14

everyOther :: Int -> Int
everyOther x
 | x < 10 = 0
 | otherwise = helper 0 x
    where
        helper result x
         | x < 10 = result
         | otherwise = helper (result*10 + (mod (div x 10) 10)) (div x 100)