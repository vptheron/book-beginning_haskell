module Chapter3.Exercises where

filterOnes :: [Integer] -> [Integer]
filterOnes l = filter (\x -> x == 1) l

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber n l = filter (\x -> x == n) l

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)


