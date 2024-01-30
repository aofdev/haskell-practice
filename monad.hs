module Monad where

data MyMaybe a = No | Yes a
  deriving (Show)

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap f No = No
  fmap f (Yes x) = Yes (f x)

instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure = Yes

  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (<*>) No _ = No
  (<*>) _ No = No
  (<*>) (Yes f) (Yes x) = Yes (f x)

instance Monad MyMaybe where
  (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  (>>=) No _ = No
  (>>=) (Yes x) f = f x

-- ghci> :i Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   {-# MINIMAL (>>=) #-}
--         -- Defined in ‘GHC.Base’

safeDiv :: Int -> Int -> MyMaybe Int
safeDiv _ 0 = No
safeDiv x y = Yes (div x y)

safeMult :: Int -> Int -> MyMaybe Int
safeMult _ 0 = No
safeMult x y = Yes (x * y)

computeAndContinue :: Int -> Int -> MyMaybe Int
computeAndContinue x y =
  case safeMult x y of
    No -> No
    Yes x -> case safeDiv x y of
      No -> No
      Yes x -> Yes x

-- computeAndContinue 5 2
-- step 1: safeMult 5 2
-- step 2: safeDiv 10 2
-- step 3: Yes 5

-- computeAndContinue 5 1
-- step 1: safeMult 5 1
-- step 2: Yes 5

-- computeAndContinue 5 0
-- step 1: safeMult 5 0
-- step 2: No

computeAndContinueMonad :: Int -> Int -> MyMaybe Int
computeAndContinueMonad x y =
  safeMult x y >>= \x ->
    safeDiv x y >>= \n ->
      Yes n

-- computeAndContinueMonad 5 2
-- step 1: safeMult 5 2
-- step 2: (\x -> safeDiv x y) 10
-- step 3: safeDiv 10 2
-- step 4: Yes 5

-- computeAndContinueMonad 5 1
-- step 1: safeMult 5 1
-- step 2: (\x -> safeDiv x y) 5
-- step 3: safeDiv 5 1
-- step 4: Yes 5

-- computeAndContinueMonad 5 0
-- step 1: safeMult 5 0
-- step 2: No

-- do notation
computeAndContinueMonadDo :: Int -> Int -> MyMaybe Int
computeAndContinueMonadDo x y = do
  x <- safeMult x y
  n <- safeDiv x y
  Yes n

-- computeAndContinueMonadDo 5 2
-- step 1: safeMult 5 2
-- step 2: x <- 10
-- step 3: safeDiv x y
-- step 4: n <- 5
-- step 5: Yes 5

-- computeAndContinueMonadDo 5 1
-- step 1: safeMult 5 1
-- step 2: x <- 5
-- step 3: safeDiv x y
-- step 4: n <- 5
-- step 5: Yes 5

-- computeAndContinueMonadDo 5 0
-- step 1: safeMult 5 0
-- step 2: No

-- Functor, Applicative, Monad
-- Functor: map over a value in a context
-- (+ 2) <$> Yes 5
-- step 1: fmap (+ 2) (Yes 5)
-- step 2: Yes ((+ 2) 5)
-- step 3: Yes 7

-- Applicative: map over a value in a context with a function in a context
-- (+) <$> Yes 5 <*> Yes 10
-- step 1: pure (+) <*> Yes 5 <*> Yes 10
-- step 2: Yes (+) <*> Yes 5 <*> Yes 10
-- step 3: Yes ((+) 5) <*> Yes 10
-- step 4: Yes 15

-- Monad: map over a value in a context with a function in a context
-- and return a value in a context
-- (+) <$> Yes 5 <*> Yes 10 >>= (\x -> Yes (x ^ 2))
-- step 1: pure (+) <*> Yes 5 <*> Yes 10 >>= (\x -> Yes (x ^ 2))
-- step 2: Yes (+) <*> Yes 5 <*> Yes 10 >>= (\x -> Yes (x ^ 2))
-- step 3: Yes ((+) 5) <*> Yes 10 >>= (\x -> Yes (x ^ 2))
-- step 4: Yes 15 >>= (\x -> Yes (x ^ 2))
-- step 5: (\x -> Yes (x ^ 2)) 15
-- step 6: Yes (15 ^ 2)
-- step 7: Yes 225

-- Monad helps us to chain computations that may fail
-- (+) <$> Yes 5 <*> No >>= (\x -> Yes (x ^ 2))
-- step 1: pure (+) <*> Yes 5 <*> No >>= (\x -> Yes (x ^ 2))
-- step 2: Yes (+) <*> Yes 5 <*> No >>= (\x -> Yes (x ^ 2))
-- step 3: Yes ((+) 5) <*> No >>= (\x -> Yes (x ^ 2))
-- step 4: No >>= (\x -> Yes (x ^ 2))
-- step 5: No