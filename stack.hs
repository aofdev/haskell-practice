module Stack where

data Stack a = EmptyStack | Stack a (Stack a) deriving (Show, Eq)

push :: a -> Stack a -> Stack a
push = Stack

pop :: Stack a -> Stack a 
pop EmptyStack = error "cannot pop empty stack"
pop (Stack _ s) = s

top :: Stack a -> a
top EmptyStack = error "cannot top empty stack"
top (Stack x _) = x


newtype StackL a = StackL [a] deriving (Show)

pushL :: a -> StackL a -> StackL a
pushL x (StackL xs) = StackL (x:xs)

popL :: StackL a -> StackL a
popL (StackL []) = error "cannot pop empty stack"
popL (StackL (_:xs)) = StackL xs

topL :: StackL a -> a
topL (StackL []) = error "cannot top empty stack"
topL (StackL (x:_)) = x