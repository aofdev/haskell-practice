
module Folding where

-- anonymous function
-- \x -> x + 1

-- map anonymous function
-- map (\x -> x + 1) [1,2,3]

-- filter anonymous function
-- filter (\x -> x > 2) [1,2,3]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f accumulator []     = accumulator
foldr' f accumulator (x:xs) = f x (foldr' f accumulator xs)

-- foldr (+) 0 [1,2,3]
-- step by step
-- foldr (+) 0 [1,2,3]
-- foldr (+) 0 (1:[2,3])
-- foldr (+) (1 + 0) [2,3]
-- foldr (+) (1 + 0) (2:[3])
-- foldr (+) (2 + (1 + 0)) [3]
-- foldr (+) (2 + (1 + 0)) (3:[])
-- foldr (+) (3 + (2 + (1 + 0))) []
-- 3 + (2 + (1 + 0))
-- 3 + (2 + 1)
-- 3 + 3
-- 6
-- right to left => foldr 

-- foldl (-) 0 [1,2,3]
-- step by step
-- foldl (-) 0 [1,2,3]
-- foldl (-) (0 - 1) [2,3]
-- foldl (-) ((0 - 1) - 2) [3]
-- foldl (-) (((0 - 1) - 2) - 3) []
-- (((0 - 1) - 2) - 3)
-- ((-1 - 2) - 3)
-- (-3 - 3)
-- -6
-- left to right => foldl


-- Map foldr
map' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- example
-- map' (* 2) [1,2,3]

-- Filter foldr
filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

-- example
-- filter' (> 2) [1,2,3]