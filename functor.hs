module Functor where

import Data.Char (toUpper)

data MyMaybe a = No | Yes a
  deriving (Show)

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap f No = No
  fmap f (Yes x) = Yes (f x)

-- :i Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--         -- Defined in ‘GHC.Base’

message :: [Char]
message = "Hello, World!"

upperMessage :: [Char]
upperMessage = fmap toUpper message

upperMessageMyMaybe :: MyMaybe [Char]
upperMessageMyMaybe = fmap (fmap toUpper) (Yes message)

upperMessageMyMaybe' :: MyMaybe [Char]
upperMessageMyMaybe' = fmap toUpper <$> Yes message

data Chain a
  = GenesisBlock
  | Block (Chain a) a
  deriving (Show)

initChain :: Chain Int
initChain = Block (Block (Block GenesisBlock 1) 2) 3

instance Functor Chain where
  fmap :: (a -> b) -> Chain a -> Chain b
  fmap f GenesisBlock = GenesisBlock
  fmap f (Block chain a) = Block (fmap f chain) (f a)