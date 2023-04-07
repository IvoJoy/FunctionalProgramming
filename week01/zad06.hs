main :: IO()
main = do

    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"

canCarry :: Int -> Double -> Double -> String
canCarry c k d
 | c<0 = error "The number of products was negative"
 | k<0 = error "John's hosting capacity was negative"
 | d<0 = error "The weight of a product was negative"
 | k >= c * d = "Yes"
 | otherwise = "No"