main :: IO()
main = do
    print $ specialSum 1 100 == 195 

specialSum :: Int -> Int -> Int
specialSum x y = sum [d | d <- [x .. y], mod (d - 1) 4 == 0 && elem '6' (show d)]