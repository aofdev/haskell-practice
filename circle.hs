module Circle where

square :: Float -> Float
square n = n * n

newtype Circle = Circle { radius :: Float } deriving (Show, Eq)

newtype Square = Square { side :: Float} deriving (Show, Eq)

class Shape a where
  area :: a -> Float

instance Shape Circle where
  area :: Circle -> Float
  area c = pi * square (radius c)

instance Shape Square where
  area :: Square -> Float
  area  s = square (side s)

data Donut = Donut { outerCircle :: Float, innerCircle :: Float } deriving (Show, Eq)

