module Board
(
    Coord,
    Size,
    Domain(..),
    Board,
    dimension,
    toDomain,
    makeBoard,
    getDomain,
    setDomain,
    setDomainRaw,
    getAffected,
    listIndices,
) where

import Data.Array
import qualified Data.Set as Set
import Util

type Coord = (Int, Int)
type Size = Int
data Domain = Invalid | Known Int | Possible (Set.Set Int) deriving (Eq, Show)

toDomain :: Set.Set Int -> Domain
toDomain s | Set.size s == 0 = Invalid
           | Set.size s == 1 = Known (Set.elemAt 0 s)
           | otherwise       = Possible s

-- A board is a square grid of domains
data Board = Board
    {
        dimension :: Size,
        contents :: Array Coord Domain
    }
    deriving (Eq)
instance Show Board where
    show (Board _ c) = show c


-- Constructs a board given a side length (size). Initially all domains are "full"
makeBoard :: Size -> Board
makeBoard size = if isSquare size then Board size (listArray ((0,0), (size-1,size-1)) (repeat $ Possible $ Set.fromAscList [0..size-1]))
                                  else error "Board size must be a square number"


getDomain :: Board -> Coord -> Domain
getDomain (Board _ b) (x, y) = b ! (x, y)

setDomainRaw :: Board -> Coord -> Set.Set Int -> Board
setDomainRaw board p val = setDomain board p (toDomain val)

setDomain :: Board -> Coord -> Domain -> Board
setDomain (Board dim con) p val = Board dim (con // [(p, val)])


-- Get the set of indices whose domains could be affected by a change to the domain of the specified index
getAffected :: Board -> Coord -> Set.Set Coord
getAffected board (x, y) =  let size = dimension board
                                smallSize = isqrt size -- The size of a small square inside the board
                                -- The x/y coordinates of the top-left corner of the small square
                                smallStart@(x', y') = ((x `div` smallSize) * smallSize, (y `div` smallSize) * smallSize)
                                -- The x/y coordinates of the bottom-right corner of the small square
                                smallEnd = (x' + smallSize - 1, y' + smallSize - 1) 
                            in
                                (Set.fromList $ listIndices (x, 0) (x, size-1)) `Set.union`
                                (Set.fromList $ listIndices (0, y) (size-1, y)) `Set.union`
                                (Set.fromList $ listIndices smallStart smallEnd)


-- Generates the list of all coordinates that "fill" a rectangle specified by the inputs
listIndices :: Coord -> Coord -> [Coord]
listIndices (x1, y1) (x2, y2) = concatMap (\x -> map (\y -> (x, y)) [y1..y2]) [x1..x2]