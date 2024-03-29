main :: IO()
main = do

    print $ height numberBTree == 4
    print $ height charBTree == 3

    print $ average numberBTree == 16.22
    --print $ average charBTree -- should not work

    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

height :: BTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

size :: BTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

roundTwoDigit :: Double -> Double
roundTwoDigit = (/ 100) . fromIntegral . round . (* 100)

average :: BTree Int -> Double
average t = roundTwoDigit $ fromIntegral (sumTree t) / fromIntegral (size t)

sumLeaves :: Num a => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node x Nil Nil) = x
sumLeaves (Node _ left right) = sumLeaves left + sumLeaves right

areEqual :: Eq a => BTree a -> BTree a -> Bool
areEqual Nil Nil = True
areEqual (Node x1 left1 right1) (Node x2 left2 right2) =
  x1 == x2 && areEqual left1 left2 && areEqual right1 right2
areEqual _ _ = False


setLevels :: BTree a -> BTree (Int, a)
setLevels = go 0
  where
    go _ Nil = Nil
    go level (Node x left right) =
      Node (level, x) (go (level + 1) left) (go (level + 1) right)


mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)


foldTree :: (a -> b) -> (b -> b -> b) -> BTree a -> b
foldTree f _ Nil = error "Empty tree"
foldTree f _ (Node x Nil Nil) = f x
foldTree f combine (Node x left right) = combine (foldTree f combine left) (foldTree f combine right)
