main :: IO()
main = do

    print $ countPalindromes 5 13 == 5
    print $ countPalindromes 13 5 == 5


isPalindrome :: Int -> Bool
isPalindrome n = rev n == n

rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)

countPalindromes :: Int -> Int -> Int
countPalindromes x y = helper (min x y) (max x y)
 where
    helper realStart realEnd 
     | realStart == realEnd = 0
     | isPalindrome (realStart + 1) = 1 + countPalindromes (realStart + 1) realEnd
     | otherwise = countPalindromes (realStart + 1) realEnd

 