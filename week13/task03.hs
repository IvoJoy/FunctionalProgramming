import Data.List
main :: IO()
main = do
 print $ levelSum numberBTree 1 == 11 -- (5 + 6)
 print $ cone numberBTree == True

cone :: BTree Int -> Bool
cone tree = levelSums tree == sort (levelSums tree)
 where
    levelSums tree = map (\k -> levelSum tree k) [0..numLevels tree - 1]

numLevels :: BTree a -> Int
numLevels Nil = 0
numLevels (Node _ left right) = 1 + max (numLevels left) (numLevels right)

levelSum :: BTree Int -> Int -> Int
levelSum tree k = sum $ getLevel tree k

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

numberBTree :: BTree Int
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Eq, Show)