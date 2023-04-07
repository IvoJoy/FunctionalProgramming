main :: IO()
main = do

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True  

    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True


isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs = (not $ null xs) && (n == head xs || isPresentRecPM n xs)



isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM n (x:xs) = n == x || isPresentRecPM n xs


isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc = elem