main :: IO()
main = do
    
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsRec :: Int -> Int
countDigitsRec 0 = 0
countDigitsRec n
 | n < 0 = error "n was negative"
 | otherwise = 1 + countDigitsRec (div n 10)

countDigitsIter :: Int -> Int
countDigitsIter n
 | n < 0 = error "n was negative"
 | otherwise = helper n
    where
        helper 0 = 0
        helper n = 1 + helper (div n 10)
