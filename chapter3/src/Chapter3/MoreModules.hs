module Chapter3.MoreModules where

import Data.List

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter =
  filter (\l -> head l == letter) . permutations

data Range = Range Integer Integer
           deriving Show

range :: Integer -> Integer -> Range
range a b
  | a <= b    = Range a b
  | otherwise = error "a must be <= b"