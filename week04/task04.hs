main :: IO()
main = do

    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565

    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565


sumUnevenLC :: Int -> Int -> Int
sumUnevenLC x y = sum [ x | x <- [min x y .. max x y], odd x] -- lines 64, 266 and 268

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF x y = sum $ filter (\ x -> odd x) [min x y .. max x y] -- here is the $
