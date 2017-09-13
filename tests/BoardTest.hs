module BoardTest
(
    boardTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Set as Set

import Data.List (sort)

import Board

boardTests :: TestTree
boardTests = testGroup "Board Tests" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    [

units :: TestTree
units = testGroup "Unit Tests"
    [
        testGroup "toDomain"
        [
            testCase "[]" $
                toDomain (Set.fromList []) @?= Invalid,
            testCase "[5]" $
                toDomain (Set.fromList [5]) @?= Known 5,
            testCase "[9,7,3]" $
                toDomain (Set.fromList [9,7,3]) @?= Possible (Set.fromList [9,7,3])
        ],
        testGroup "setDomain"
        [
            testCase "Invalid" $
                getDomain (setDomain (makeBoard 1) (0,0) Invalid) (0,0) @?= Invalid,
            testCase "Known 7" $
                getDomain (setDomain (makeBoard 4) (1,3) (Known 7)) (1,3) @?= Known 7,
            testCase "Possible [1,4,8]" $
                getDomain (setDomain (makeBoard 9) (6,2) (Possible $ Set.fromList [1,4,8])) (6,2) @?= Possible (Set.fromList [1,4,8])
        ],
        testGroup "getAffected"
        [
            testCase "Size 4, Position (0, 0)" $
                getAffected (makeBoard 4) (0, 0) @?= Set.fromList [(0, 0), (0, 1), (1, 0), (1, 1), (0, 0), (0, 1), (0, 2), (0, 3), (0, 0), (1, 0), (2, 0), (3, 0)],
            testCase "Size 4, Position (2, 1)" $
                getAffected (makeBoard 4) (2, 1) @?= Set.fromList [(2, 0), (3, 0), (2, 1), (3, 1), (2, 0), (2, 1), (2, 2), (2, 3), (0, 1), (1, 1), (2, 1), (3, 1)],
            testCase "Size 9, Position (0, 0)" $
                getAffected (makeBoard 9) (0, 0) @?= Set.fromList [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2), (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0)],
            testCase "Size 9, Position (5, 3)" $
                getAffected (makeBoard 9) (5, 3) @?= Set.fromList [(3, 3), (3, 4), (3, 5), (4, 3), (4, 4), (4, 5), (5, 3), (5, 4), (5, 5), (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (5, 7), (5, 8), (0, 3), (1, 3), (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3)]
        ],
        testGroup "listIndices"
        [
            testCase "Single cell" $
                listIndices (0, 0) (0, 0) @?= [(0,0)],
            testCase "Row" $
                listIndices (0, 0) (5, 0) @?= [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)],
            testCase "Column" $
                listIndices (0, 0) (0, 5) @?= [(0,0), (0,1), (0,2), (0,3), (0,4), (0,5)],
            testCase "Rectangle" $
                listIndices (0, 0) (1, 1) @?= [(0,0), (0,1), (1,0), (1,1)]
        ]
    ]