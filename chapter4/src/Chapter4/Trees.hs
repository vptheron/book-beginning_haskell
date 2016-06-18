module Trees where

import Data.Tree

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 [] ]

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v forest) =
  let subtreesResult =
        concatMap (preOrder f) forest
  in (f v) : subtreesResult