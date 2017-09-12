module DistinctQueueTest
(
    distinctQueueTests
) where

import Test.Tasty;
import Test.Tasty.HUnit;
import Test.Tasty.QuickCheck;

import DistinctQueue

import Data.List (nub)

distinctQueueTests :: TestTree
distinctQueueTests = testGroup "DistinctQueue Tests" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    [
        testProperty "Right length after duplicates removed" $
            (\list -> size (pushList empty list) == (length . nub) (list :: [Int])),
        testProperty "Duplicates removed" $
            (\list -> toList (pushList empty list) == nub (list :: [Int]))
    ]

units :: TestTree
units = testGroup "Unit Tests"
    [
        
    ]