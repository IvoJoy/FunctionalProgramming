main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

t1 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (4,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))
t2 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

ordered :: Ord a => BTree (a, a) -> Bool
ordered Nil = True
ordered (Node (x, y) left right) =
    (all (\(a, b) -> a >= x && b <= y) (inOrder left)) &&
    (all (\(a, b) -> a <= x && b >= y) (inOrder right)) &&
    ordered left &&
    ordered right
  where
    inOrder Nil = []
    inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r