module UtilTest
(
    utilTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Util

utilTests :: TestTree
utilTests = testGroup "Util Tests" [properties, units]

properties :: TestTree
properties = testGroup "Properties"
    [

    ]

units :: TestTree
units = testGroup "Unit Tests"
    [
        testGroup "isSquare"
        [
            testCase "4" $ isSquare 4 @?= True,
            testCase "9" $ isSquare 9 @?= True,
            testCase "8" $ isSquare 8 @?= False,
            testCase "97" $ isSquare 97 @?= False
        ],
        testGroup "isqrt"
        [
            testCase "1" $ isqrt 1 @?= 1,
            testCase "4" $ isqrt 4 @?= 2,
            testCase "9" $ isqrt 9 @?= 3,
            testCase "81" $ isqrt 81 @?= 9,
            testCase "5" $ isqrt 5 @?= 2,
            testCase "90" $ isqrt 90 @?= 9,
            testCase "1642" $ isqrt 1642 @?= 40
        ]
    ]