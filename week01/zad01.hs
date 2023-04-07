main :: IO()
main = do

    print $ isEvenIf 2
    print $ isEvenIf 15452
    print $ isEvenIf 321

    print $ isEvenGuards 2
    print $ isEvenGuards 15452 
    print $ isEvenGuards 321

isEvenIf :: Int -> String
isEvenIf n = if mod n 2 == 0 then "Yes" else "No"

isEvenGuards :: Int -> String
isEvenGuards n
 | mod n 2 == 0 = "Yes"
 | otherwise = "No"
