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

-- pretty printing functions
validityStr :: Bool -> String
validityStr v = if v then "Valid" else "Invalid"

-- keep only integers
filterFormatting str = filter (\x -> x `elem` ['0'..'9']) str

-- generates a test case
pointInRangeTester p1 p2 tf = ((show p1) ++ " " ++ (show p2)) ~: tf ~=? (SS.pointInRange p1 p2)

-- two basic test cases
pointInRange1 = pointInRangeTester (0,0) (1,1) True
pointInRange2 = pointInRangeTester (0,0) (8,7) False

readFileTest fn isValid = "[Board " ++ fn ++ "] Testing sudoku file read " ~:
                          do
                            f <- readFile $ pathJoin ["boards", fn]
                            case SR.readSudokuFile f of
                              Just board -> assertBool ((validityStr isValid) ++ " can read file") isValid
                              _ -> assertBool ((validityStr (not isValid)) ++ " can't read file") $ not isValid

readFileTest1 = readFileTest "board1.txt" True
readFileTest2 = readFileTest "board2.txt" True
readFileTest3 = readFileTest "invalidboard1.txt" False

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

compareSolved boardName = "Comparing " ++ boardName ++ ".txt with " ++ boardName ++ ".ans" ~:
                          let fn = pathJoin ["boards", boardName]
                              filterS = liftM filterFormatting in
                          do
                            puzzle <- readFile $ fn ++ ".txt"
                            correct <- filterS $ readFile $ fn ++ ".ans"
                            case SR.readSudokuFile puzzle of
                              Just board -> case SS.solve board of
                                Just answer -> assertBool "Answer not the same" $ filterFormatting (SS.sudokuToStr answer) == correct
                                _ -> assertFailure "Could not solve board when solution is possible"
                              _ -> assertFailure $ "Could not read Sudoku file " ++ fn ++ ".txt"

options1 = optionsTests (0,1) [1,3,4,6] "board1.txt"
options2 = optionsTests (8,0) [1,2,7,8] "board1.txt"
noOptions = optionsTests (3,3) [] "board1.ans"

allSSTests = [TestLabel "pointInRange1" pointInRange1, 
              TestLabel "pointInRange2" pointInRange2, 
              TestLabel "compareSolved1" $ compareSolved "board1", 
              TestLabel "compareSolved2" $ compareSolved "board2",
              TestLabel "options1" options1,
              TestLabel "options2" options2, 
              TestLabel "nooptions" noOptions]

allSRTests = [TestLabel "readFile1" readFileTest1,
              TestLabel "readFile2" readFileTest2,
              TestLabel "readFile3" readFileTest3]
             
allTests = TestList (allSSTests ++ allSRTests)

main = do runTestTT allTests
