module Chapter3.Exercises where

import Data.List

filterOnes :: [Integer] -> [Integer]
filterOnes l = filter (\x -> x == 1) l

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber n l = filter (\x -> x == n) l

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

enumUnfold :: Int -> Int -> [Int]
enumUnfold a b =
  unfoldr (\x -> if x > b then Nothing else (Just (x,x+1))) a


