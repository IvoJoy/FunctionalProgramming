main :: IO()
main = do
    
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

isPalindrome :: Int -> Bool
isPalindrome n = rev n == n

rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)

smallestPalindromeDivisor :: Int -> Int
smallestPalindromeDivisor n = filter (\x -> isPalindrome x && mod n x == 0) [2 .. n] !! 0

greatestPalindromeDivisor :: Int -> Int
greatestPalindromeDivisor n = filter (\x -> isPalindrome x && mod n x == 0) (reverse [2 .. n]) !! 0

getPalindromes :: Int -> Int
getPalindromes n = smallestPalindromeDivisor n + greatestPalindromeDivisor n


