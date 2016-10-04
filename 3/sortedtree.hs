-- sortedtree.hs
-- Jeremy.Singer@glasgow.ac.uk
-- Example code for #FLhaskell course

-- Nodes contain integers, Leaves are empty
data Tree = Leaf | Node Int Tree Tree deriving Show



treeDepth :: Tree -> Int
-- longest path from root to a leaf
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)


isSortedTree :: Tree -> Int -> Int -> Bool
-- is the tree sorted in-order?
-- the two Int params indicate min and max
-- for the current subtree
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted



addNewMax :: Tree -> Tree
-- add a new max element to tree
-- will go down rightmost path to Leaf
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree



treeSum :: Tree -> Int
-- sum the values of all the nodes of the tree
treeSum Leaf = 0
treeSum (Node x left right) =
    x + (treeSum left) + (treeSum right)



insert :: Int -> Tree -> Tree
-- insert new element to the tree in ascending order
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
    | x < y = Node y (insert x left) right
    | otherwise = Node y left (insert x right)



toList :: Tree -> [Int]
-- convert tree to list
toList Leaf = []
toList (Node x left right) =
    (toList left) ++ [x] ++ (toList right)



t1 = insert 5 Leaf
t2 = insert 4 t1
t3 = insert 3 t2
t4 = insert 7 t3
t5 = insert 2 t4
t6 = insert 10 t5

tree = (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
