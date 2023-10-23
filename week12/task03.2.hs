main :: IO()
main = do
  
  print $ maxDepthBlueNode colorTree -- == 2

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode tree = traverse tree 0
 where
   traverse Empty depth = depth
   traverse (Node color left right) depth
     | color == Blue = max leftDepth rightDepth
     | otherwise = max leftDepth' rightDepth'
       where
        leftDepth' = traverse left depth
        rightDepth' = traverse right depth
        leftDepth = traverse left (depth + 1)
        rightDepth = traverse right (depth + 1)

data Color = Red | Green | Blue 
 deriving (Eq)
data Tree = Empty | Node Color Tree Tree 
 deriving (Eq)

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)