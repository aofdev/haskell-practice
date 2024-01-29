module TypeClasses where

data Score = Score
  { pair :: (Double, Double)
  }

-- :i Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}
-- Defined in ‘GHC.Classes’

instance Eq Score where
  (==) (Score (a1, b1)) (Score (a2, b2))
    | a1 == a2 && b1 == b2 = True
    | otherwise = False

-- :i Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--         -- Defined in ‘GHC.Num’
instance Num Score where
  (+) (Score (a1, b1)) (Score (a2, b2)) = Score (a1 + a2, b1 + b2)
  (-) (Score (a1, b1)) (Score (a2, b2)) = Score (a1 - a2, b1 - b2)
  (*) (Score (a1, b1)) (Score (a2, b2)) = Score (a1 * a2, b1 * b2)

instance Show Score where
  show (Score (a, b)) = "Score: " ++ show a ++ ", " ++ show b

class ScoreTotal a where
  score :: a -> Double

instance ScoreTotal Score where
  score (Score (a, b)) = a + b

scoreA :: Score
scoreA = Score (1.0, 2.0)

scoreB :: Score
scoreB = Score (3.0, 4.0)
