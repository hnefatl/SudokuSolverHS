module DistinctQueue
(
    DistinctQueue,
    empty,
    size,
    front,
    push,
    pushList,
    pop,
    toList
) where

import qualified Data.Set as S

data DistinctQueue a = DistinctQueue
    {
        distinct :: S.Set a,
        begin :: [a],
        end :: [a]
    }
    deriving (Eq)

empty :: DistinctQueue a
empty = DistinctQueue S.empty [] []

size :: Ord a => DistinctQueue a -> Int
size (DistinctQueue d _ _) = S.size d

-- Returns (non-destructively) the element from the front of the queue
front :: Ord a => DistinctQueue a -> a
front (DistinctQueue _ [] _) = error "Empty queue"
front (DistinctQueue _ (x:_) _) = x

-- Inserts an item to the back of the queue, if it's not already contained in the queue
push :: Ord a => DistinctQueue a -> a -> DistinctQueue a
push q@(DistinctQueue d f b) v = if S.member v d then q
                                                 else DistinctQueue (S.insert v d) f (v:b)


pushList :: Ord a => DistinctQueue a -> [a] -> DistinctQueue a
pushList q [] = q
pushList q (x:xl) = pushList (push q x) xl

-- Removes an item from the front of the queue. 
-- Invariant: if the length of the queue is at least 2 before the pop operation, then afterwards the "front list" will be non-empty
pop :: Ord a => DistinctQueue a -> DistinctQueue a
pop (DistinctQueue _ [] []) = error "Empty queue"
pop (DistinctQueue d [] b) = pop (DistinctQueue d (reverse b) [])
pop (DistinctQueue d [x] b) = DistinctQueue (S.delete x d) b []
pop (DistinctQueue d (x:f) b) = DistinctQueue (S.delete x d) f b

-- Returns the contents of the queue as a list, in order
toList :: Ord a => DistinctQueue a -> [a]
toList (DistinctQueue _ f b) = f ++ reverse b
