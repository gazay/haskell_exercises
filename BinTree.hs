module BinTree where
  import Nat

  data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
    deriving (Show)

  reverse :: BinTree a -> BinTree a
  reverse (Leaf a) = Leaf a
  reverse (Node a b) = Node b a

  depth :: BinTree a -> Nat
  depth (Leaf a) = Zero
  depth (Node x y) = Succ (max (depth x) (depth y))

  leaves :: BinTree a -> [a]
  leaves (Leaf a) = [a]
  leaves (Node x y) = (leaves x) ++ (leaves y)
