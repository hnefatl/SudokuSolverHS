module Main
(
    main
) where

import Board

main :: IO ()
main = print $ getAffected (makeBoard 9) (5, 3)