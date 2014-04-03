module SudokuTests where

import Test.HUnit

import qualified SudokuSolver as SS
import qualified SudokuReader as SR

pointInRangeTester p1 p2 tf = TestCase $ assertEqual ((show p1) ++ " " ++ (show p2)) tf $ SS.pointInRange p1 p2

pointInRange1 = pointInRangeTester (0,0) (1,1) True
pointInRange2 = pointInRangeTester (0,0) (8,7) False

allSSTests = TestList [TestLabel "pointInRange1" pointInRange1, TestLabel "pointInRange2" pointInRange2]

main = do runTestTT allSSTests