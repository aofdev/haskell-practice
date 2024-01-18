module Lists where

-- list 
a :: [Integer]
a = [1, 2, 3]

-- range
a' :: [Integer]
a' = [1..3] -- [1, 2, 3]

-- range with step 
a'' :: [Integer]
a'' = [1, 3..20] -- [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]

-- list comprehension and predicate -- [f x | x <- xs, p x]
b :: [Integer]
b = [x ^ 2 | x <- [1..10], even x] -- [4, 16, 36, 64, 100]
-- predicate: even x
-- [x ^ 2 | x <- [1..10], even x]
-- [x ^ 2 | x <- [2, 4, 6, 8, 10]]
-- [4, 16, 36, 64, 100]

-- concat list: ++
-- [1,2,3,4,5] ++ [6,7,8,9,10]
-- [1,2,3,4,5,6,7,8,9,10]

-- access list element by index: !!
-- [1,2,3,4,5] !! 2
-- 3

-- head and tail
-- head [1,2,3,4,5]
-- 1
-- tail [1,2,3,4,5]
-- [2,3,4,5]

-- :type
-- :type (++)
-- (++) :: [a] -> [a] -> [a]

-- :type (!!)
-- (!!) :: [a] -> Int -> a


-- cons operator: (:)
c :: [Integer]
c = 1 : 2 : [3, 4, 5] -- [1, 2, 3, 4, 5]
-- :type (:)
-- (:) :: a -> [a] -> [a]
