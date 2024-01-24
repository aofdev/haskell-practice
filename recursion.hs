module Recursion where

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x : xs) = x + sumList xs

-- sumList [1, 2, 3]
-- 1 + sumList [2, 3]
-- 1 + (2 + sumList [3])
-- 1 + (2 + (3 + sumList []))
-- 1 + (2 + (3 + 0))
-- 1 + (2 + 3)
-- 1 + 5
-- 6

sumList' :: [Integer] -> Integer
sumList' [] = 1
sumList' elements = head elements + sumList' (tail elements)

-- if and else
sumListWithCondition :: [Integer] -> Integer
sumListWithCondition [] = 0
sumListWithCondition (x : xs) = if even x then x + sumListWithCondition xs else sumListWithCondition xs

-- guards
sumListWithCondition' :: [Integer] -> Integer
sumListWithCondition' [] = 0
sumListWithCondition' (x : xs)
  | even x = x + sumListWithCondition' xs
  | otherwise = sumListWithCondition' xs


-- reverse list
reverseList :: [o] -> [o]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

-- reverseList [1, 2, 3]
-- reverseList [2, 3] ++ [1]
-- (reverseList [3] ++ [2]) ++ [1]
-- ((reverseList [] ++ [3]) ++ [2]) ++ [1]
-- ((([] ++ [3]) ++ [2]) ++ [1])
-- [3, 2, 1]

-- find number in list
findNumber :: Integer -> [Integer] -> Bool
findNumber n [] = False
findNumber n (x : xs) 
  | n == x  = True
  | otherwise = findNumber n xs

-- findNumber 3 [1, 2, 3]
-- findNumber 3 [2, 3]
-- findNumber 3 [3]
-- True
