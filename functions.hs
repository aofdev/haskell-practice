module Functions where

a :: Integer
a = 1+1 -- infix operator

a' :: Integer
a' = (+) 1 2 -- prefix operator

b :: Double
b = 4/2

b' :: Double
b' = (/) 4 2

c :: Int
c = 4 `div` 2

c' :: Int
c' = div 4 2


d = 2 ^ (3^4) + 1

-- :i (:)
-- type [] :: * -> *
-- data [] a = ... | a : [a]
--         -- Defined in ‘GHC.Types’
-- infixr 5 :
-- right associativity
-- 1 : 2 : [3, 4, 5]
-- 1 : (2 : [3, 4, 5])

-- associative
-- (1 : 2 : [3, 4, 5]) ++ [6, 7, 8]
-- 1 : (2 : [3, 4, 5] ++ [6, 7, 8])
-- 1 : (2 : [3, 4, 5, 6, 7, 8])
-- 1 : 2 : [3, 4, 5, 6, 7, 8]
-- 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : []

-- precedence
-- 2 ^ 3 ^ 4 + 1
-- 2 ^ (3 ^ 4) + 1


-- Function definition -- f x = x + 1
hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (a^2 + b^2)


-- Function composition -- (f . g) x = f (g x)
double :: Integer -> Integer
double x = x * 2

square :: Integer -> Integer
square x = x ^ 2

doubleSquare :: Integer -> Integer
doubleSquare = square . double


-- currying -- f x y = f (x, y)
add :: Integer -> Integer -> Integer
add x y = x + y

addTwo :: Integer -> Integer
addTwo = add 2

addThree :: Integer -> Integer
addThree = add 3