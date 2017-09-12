module Util
(
    isSquare,
    isqrt,
) where

isSquare :: Int -> Bool
isSquare x = let root = sqrt (fromIntegral x :: Double) in
                root * root == fromIntegral x

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral