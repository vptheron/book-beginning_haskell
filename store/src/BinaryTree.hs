module BinaryTree where

data TravelGuide = TravelGuide {
                     title :: String,
                     authors :: [String],
                     price :: Double }
                 deriving (Show, Eq, Ord)

data BinaryTree = Node TravelGuide BinaryTree BinaryTree
                | Leaf
                deriving Show

treeFind :: TravelGuide -> BinaryTree -> Maybe TravelGuide
treeFind t (Node tg l r)
  | t == tg = Just tg
  | t < tg = treeFind t l
  | t > tg = treeFind t r
treeFind _ Leaf = Nothing