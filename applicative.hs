module Applicative where

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


-- (+) <$> (Yes 5) <*> (Yes 10)
-- Yes 15

-- ghci> :t ((+) <$> (Yes 5))
-- ((+) <$> (Yes 5)) :: Num a => MyMaybe (a -> a)

-- (\x y z -> x + y + z) <$> (Yes 4) <*> (Yes 1) <*> (Yes 10)
-- Yes 15

-- pure (* 5) <*> (Yes 2)
-- Yes 10

-- pure (* 5) <*> No
-- No

-- pure (+) <*> (Yes 5) <*> (Yes 10)
-- Yes 15