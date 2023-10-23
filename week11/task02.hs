main :: IO()
main = do

    print $ minDepthGreenNode colorTree == 2


data Color = Red | Green | Blue
 deriving (Show, Eq)
data Tree = Empty | Node Color Tree Tree
 deriving (Show)

colorTree :: Tree
colorTree = Node Blue (Node Red(Node Green Empty Empty)Empty)(Node Red(Node Blue(Node Green Empty Empty)(Node Red Empty Empty))Empty)

minDepthGreenNode :: Tree -> Int
minDepthGreenNode Empty = 0
minDepthGreenNode (Node color left right)
 | color == Green = 0
 | otherwise = 1 + min (minDepthGreenNode left) (minDepthGreenNode right)


