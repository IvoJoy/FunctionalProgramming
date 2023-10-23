main :: IO()
main = do

    print $ isGraceful t1 == True
    print $ isGraceful t2 == False

data NTree a = Nil | Node a [NTree a]  
   
t1 :: NTree Int  
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]] 
  
t2 :: NTree Int 
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]

isGraceful :: NTree Int -> Bool
isGraceful Nil = True
isGraceful (Node _ [Nil]) = True
isGraceful (Node value nodes) = all (\x -> even $ abs (value - x)) (map (\(Node z _) -> z) nodes) && all (\y -> isGraceful y) nodes

