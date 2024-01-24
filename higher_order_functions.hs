module HigherOrderFunctions where

import Control.Arrow (ArrowApply (app))

squareAllElements :: [Integer] -> [Integer]
squareAllElements [] = []
squareAllElements (x : xs) = x * x : squareAllElements xs

minusOneAllElements :: [Integer] -> [Integer]
minusOneAllElements [] = []
minusOneAllElements (x : xs) = x - 1 : minusOneAllElements xs

applyToAllElements :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToAllElements f [] = []
applyToAllElements f (x : xs) = f x : applyToAllElements f xs

applyToAllElements' :: (a -> b) -> [a] -> [b]
applyToAllElements' f [] = []
applyToAllElements' f (x : xs) = f x : applyToAllElements' f xs

-- :t map
-- map :: (a -> b) -> [a] -> [b]
applyMapToAllElements :: (a -> b) -> [a] -> [b]
applyMapToAllElements f [] = []
applyMapToAllElements f (x : xs) = map f (x : xs)


-- import Data.Char;
-- applyToAllElements' toUpper "hello"
-- "HELLO"

-- applyToAllElements' toUpper ['a'..'z']
-- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicate [] = []
myFilter predicate (x : xs)
  | predicate x = x : myFilter predicate xs
  | otherwise = myFilter predicate xs

-- myFilter even [1, 2, 3, 4, 5]
-- [2, 4]

-- myFilter odd [1, 2, 3, 4, 5]
-- [1, 3, 5]

-- map (*2) (filter odd [1..10])
-- filter odd [1..10]
-- [1, 3, 5, 7, 9]
-- map (*2) [1, 3, 5, 7, 9]
-- [2, 6, 10, 14, 18]
