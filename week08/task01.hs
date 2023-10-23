import Data.List

main :: IO()
main = do

    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]] 
    
    print $ simplePaths [(1, [2, 3, 4]), (2, [3, 4]), (3, []), (4, [])] 1 1
    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 1 2

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

getChildren :: Graph -> Node -> [Node] 
getChildren g n = head [ch | (p, ch) <- g, p == n]  

isThereAPath :: Graph -> Path -> Bool 
isThereAPath g path = all (\ (p, ch) -> elem ch (getChildren g p)) $ zip path (tail path)  

simplePaths :: Graph -> Node -> Node -> [Path] 
simplePaths graph k n = [xs | xs <- subsequences nodes, length xs == k + 1 && isThereAPath graph xs, head xs == n]  
 where     
    nodes = map fst graph