import Data.List
main :: IO()
main = do
 print $ leavesAreEqual t1 t2 == True
 print $ leavesAreEqual t1 t3 == False

leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual bt1 bt2 = sort (leaves bt1) == sort (leaves bt2) 

leaves :: BTree -> [Int]
leaves Nil = []
leaves (Node val Nil Nil) = [val]
leaves (Node _ left right) = leaves left ++ leaves right

t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))

t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

data BTree = Nil | Node Int BTree BTree