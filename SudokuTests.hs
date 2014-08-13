module SudokuTests where

import Test.HUnit
import Data.List
import Data.Array
import System.FilePath
import Control.Monad

import qualified SudokuSolver as SS
import qualified SudokuReader as SR

-- utility functions
pathJoin dirs = intercalate [pathSeparator] dirs

-- keep only integers
filterFormatting str = filter (\x -> x `elem` ['0'..'9']) str

-- generates a test case
pointInRangeTester p1 p2 tf = ((show p1) ++ " " ++ (show p2)) ~: tf ~=? (SS.pointInRange p1 p2)

-- two basic test cases
pointInRange1 = pointInRangeTester (0,0) (1,1) True
pointInRange2 = pointInRangeTester (0,0) (8,7) False

-- assert that opts must equal calculated options
-- fn does not include directory

-- TODO: add functionality to ensure point tested on is set to 0
-- options is not defined when ran on points not set to 0
optionsTests pt opts fn = "[Board " ++ fn ++ "] Testing options at " ++ (show pt) ~:
                          do
                            f <- readFile $ pathJoin ["boards", fn]
                            case SR.readSudokuFile f of
                              Just board -> assertBool "Options didn't match" $ SS.options (assocs board) pt == opts
                              _ -> assertFailure $ "Could not read Sudoku file " ++ fn


-- TODO: also refactor, ugly
compareSolved boardName = "Comparing " ++ boardName ++ ".txt with " ++ boardName ++ ".ans" ~:
                          filterS (readFile (fn ++ ".txt")) >>= (\boardContents -> filterS (readFile (fn ++ ".ans")) >>=
                          (\answerContents -> case SR.readSudokuFile boardContents of 
                                           Just board -> case SS.solve board of 
                                             Just answer -> (return ((SS.sudokuToStr answer) == answerContents))
                                             _ -> return False
                                           _ -> return False)) >>= assertBool "Computed board did not match"
                          where fn = pathJoin ["boards", boardName]
                                filterS = liftM filterFormatting

options1 = optionsTests (0,1) [1,3,4,6] "board1.txt"
options2 = optionsTests (8,0) [1,2,7,8] "board1.txt"
noOptions = optionsTests (3,3) [] "board1.ans"

allSSTests = [TestLabel "pointInRange1" pointInRange1, 
              TestLabel "pointInRange2" pointInRange2, 
              TestLabel "compareSolved1" $ compareSolved "board1", 
              TestLabel "compareSolved2" $ compareSolved "board2"]

allSRTests = [TestLabel "options1" options1,
              TestLabel "options2" options2, 
              TestLabel "nooptions" noOptions]

allTests = TestList (allSSTests ++ allSRTests)

main = do runTestTT allTests
