module DataTypes where

import Prelude hiding (fst, snd)

-- Data types
-- data Bool = False | True
-- Left side of | is a type constructor
-- Right side of | is a data constructor

data Result = Fail | Success deriving (Show)

-- Result is a type constructor
-- Fail and Success are data constructors

data Member = Member Name Age Language deriving (Show)

member1 :: Member
member1 = Member "Max" 20 "TH"

-- type synonyms
type Name = String

type Age = Int

type Language = String

-- record syntax
data Member' = Member'
  { name :: Name,
    age :: Age,
    language :: Language
  }
  deriving (Show)

member1' :: Member'
member1' =
  Member'
    { name = "Max",
      age = 30,
      language = "TH"
    }

-- access record fields
-- name member1'
-- age member1'
-- language member1'

-- polymorphic data types
-- data constructor may zero or more type parameters
-- type constructor may zero or more type parameters

data Square a = Square a

area :: (Num a) => Square a -> a
area (Square a) = a * a

-- recursive data types
data Blockchain a
  = GenesisBlock
  | Block (Blockchain a) a
  deriving (Show)

initChain :: Blockchain Int
initChain = Block GenesisBlock 3

initChain2 :: Blockchain Int
initChain2 = Block (Block (Block GenesisBlock 3) 4) 5

chainLength :: Blockchain a -> Int
chainLength GenesisBlock = 0
chainLength (Block chain value) = 1 + chainLength chain

-- Built-in Datatypes

-- tuples
fst :: (a, b) -> a
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b

-- Maybe

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)


-- Either
-- :i Either
-- data Either a b = Left a | Right b

safeDivEither :: Int -> Int -> Either String Int
safeDivEither _ 0 = Left "Cannot divide by zero"
safeDivEither a b = Right (a `div` b)

safeHeadEither :: [a] -> Either String a
safeHeadEither [] = Left "Empty list"
safeHeadEither xs = Right (head xs)
