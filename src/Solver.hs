module Solver
(
    -- propagateChanges
) where

import qualified Data.Set as Set
import DistinctQueue

import Board

data State = State
    {
        
    }
    deriving (Eq, Show)


-- preprocess :: Board -> Board
-- preprocess b = preprocess' b (fromList $ listIndices (0,0) (dimension b - 1, dimension b - 1)) where
--     preprocess' b queue | Seq.empty queue = b
--                         | otherwise       = 

-- propagateChanges :: Board -> DistinctQueue Coord -> Maybe Board
-- propagateChanges b q | isEmpty q = Just b
--                      | otherwise = case getDomain b c of
--                                        Invalid     -> Nothing
--                                        Known x     -> let
--                                                           
--                                                       in
--                                                    
--                                        Possible xl ->