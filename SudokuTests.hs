module SudokuTests where

import Test.HUnit
import Data.List
import Data.Array
import System.FilePath

import qualified SudokuSolver as SS
import qualified SudokuReader as SR

-- utility functions
pathJoin dirs = intercalate [pathSeparator] dirs

-- generates a test case
pointInRangeTester p1 p2 tf = ((show p1) ++ " " ++ (show p2)) ~: tf ~=? (SS.pointInRange p1 p2)

-- two basic test cases
pointInRange1 = pointInRangeTester (0,0) (1,1) True
pointInRange2 = pointInRangeTester (0,0) (8,7) False

-- assert that opts must equal calculated options
-- fn does not include directory

-- TODO: refactor, ugly
optionsTests pt opts fn = "[Board " ++ fn ++ "] Testing options at " ++ (show pt) ~: 
                          return (pathJoin ["boards", fn]) >>= readFile >>= 
                          (\f -> case SR.readSudokuFile f of 
                              Just board -> return $ (SS.options (assocs board) pt) == opts
                              _ -> return False) >>= assertBool "Options didn't match"

options1 = optionsTests (0,1) [1,3,4,6] "board1.txt"

allSSTests = [TestLabel "pointInRange1" pointInRange1, 
              TestLabel "pointInRange2" pointInRange2]

allSRTests = [TestLabel "options1" options1]

allTests = TestList (allSSTests ++ allSRTests)

main = do runTestTT allTests
