module SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if (null lst1)
                then lst2
                else (head lst1) : (tail lst1 +++ lst2)

reverse2 :: [a] -> [a]
reverse2 lst = if (null lst)
               then lst
               else reverse2 (tail lst) +++ [head lst]

maxmin :: Ord a => [a] -> (a,a)
maxmin lst = if null (tail lst)
             then (head lst, head lst)
             else (
               if (head lst) > fst (maxmin (tail lst))
               then head lst
               else fst (maxmin (tail lst))
             ,
               if (head lst) < snd (maxmin (tail lst))
               then head lst
               else snd (maxmin (tail lst))
             )

maxmin2 :: Ord a => [a] -> (a,a)
maxmin2 lst = let h = head lst
              in if null (tail lst)
                 then (h,h)
                 else (
                   if h > t_max then h else t_max,
                   if h < t_min then h else t_min
                 ) where t = maxmin (tail lst)
                         t_max = fst t
                         t_min = snd t

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0     = Nothing
ifibonacci 0             = Just 0
ifibonacci 1             = Just 1
ifibonacci n | otherwise =
  let Just f1 = ifibonacci (n - 1)
      Just f2 = ifibonacci (n - 2)
  in Just (f1 + f2)

binom :: Integer -> Integer -> Integer
binom _ 0          = 1
binom x y | x == y = 1
binom n k          =
  (binom (n - 1) (k - 1)) + (binom (n - 1) k)

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 | m > 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 [] = ([],[])
unzip2 ((x,y) : xys) =
  let (xs,ys) = unzip(xys)
  in (x : xs, y : ys)