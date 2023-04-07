main :: IO()
main = do

    print $ (pairCompose [(+1), (+2)]) 1 == 5
    print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8

pairCompose :: [(Int -> Int)] -> (Int -> Int)
pairCompose [] = (\ x -> x)
pairCompose [f] = (\ x -> (f $ id x))
pairCompose (f1:f2:fs) = (\ x -> (f1 $ f2 x) + (pairCompose fs) x)