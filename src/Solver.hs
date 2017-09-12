module Solver
(

) where

import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Board

data State = State
    {
        
    }
    deriving (Eq, Show)


-- preprocess :: Board -> Board
-- preprocess b = preprocess' b ((Seq.fromList . Set.toList)(listIndices (0,0) (dimension b - 1, dimension b - 1))) where
--     preprocess' b queue | Seq.empty queue = b
--                         | otherwise       = 