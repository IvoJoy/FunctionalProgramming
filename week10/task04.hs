main :: IO()
main = do

    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]

type Graph = (Int, [Int])
type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

getChildren :: Tree -> Int-> Int
getChildren g x = fst $ head [(p, ch) | (p, ch) <- g, elem x ch]

getBrothers :: Tree -> Int -> [Int]
getBrothers g x = concat [ch | (p, ch) <- g, elem x ch]

findUncles :: Tree -> Int -> [Int]
findUncles tree x = let p = getChildren tree x in [ y | y <- getBrothers tree p, y /= p]


