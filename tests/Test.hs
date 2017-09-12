import Test.Tasty

import BoardTest
import UtilTest
import DistinctQueueTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [boardTests, utilTests, distinctQueueTests]