main :: IO()
main = do

    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 


findSum :: Int -> Int -> Int -> Int
findSum a b n
 | n<=3 = error "Incorrect input"
 | otherwise = sumOfTwo n b a + sumOfTwo (n-1) b a + sumOfTwo (n-2) b a


sumOfTwo :: Int -> Int -> Int -> Int
sumOfTwo 0 b sum = sum
sumOfTwo n b sum = sumOfTwo (n-1) b (sum + b * 2^(n-1))