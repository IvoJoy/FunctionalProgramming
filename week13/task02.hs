main :: IO()
main = do
 print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

convert :: BTree Int -> BTree Int
convert Nil = Nil
convert tree = helper tree (traverseDFS tree)
 where
    helper Nil _ = Nil
    helper (Node value left right) nodes = Node (sum $ filter (>= value) nodes) (helper left nodes) (helper right nodes)

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

tree :: BTree Int
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Eq, Show)