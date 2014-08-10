-- Author: David Fonenot, 2014 fontenod@onid.oregonstate.edu

-- compile with -main-is SudokuSolver to get to work with GHC

module SudokuSolver where
import SudokuReader

import System.Environment
import System.Exit
import Data.Array
import Data.List
import Data.Char
import Data.Ord

sudokuToStr :: SBoard -> String
sudokuToStr board | (bounds board) /= ((0,0),(8,8)) = "Not a valid sudoku board"
sudokuToStr board = sudokuToStr' $ assocs board
                    where
                      sudokuToStr' ((_,v):[]) = (intToDigit v):[]
                      sudokuToStr' (((r,c),v):vs) | c == 8 = (intToDigit v):'\n':sudokuToStr' vs
                      sudokuToStr' (((r,c),v):vs) = (intToDigit v):sudokuToStr' vs

-- nothing on complete sudoku board (no zeroes)
advance :: [SValue] -> (Int,Int) -> Maybe (Int,Int)
advance [] _ = Nothing
advance ((pt,_):rst) start | pt < start = advance rst start
advance ((pt,v):rst) start | pt >= start = if v == 0 then Just pt else advance rst start

-- is the point point 1 in the range of sudoku value point 2
pointInRange :: (Int,Int) -> (Int,Int) -> Bool
pointInRange (r1,c1) (r2,c2) = or [r1==r2,c1==c2,sameBox]
                               where
                                 lowRow = (r2 `div` 3) * 3
                                 highRow = lowRow + 2
                                 lowCol = (c2 `div` 3) * 3
                                 highCol = lowCol + 2
                                 sameBox = and [r1>=lowRow,r1<=highRow,c1>=lowCol,c1<=highCol]

-- not protecting against values outside of the range of the 
-- sudoku board, function meant for being called internally only
selectVal :: [SValue] -> (Int,Int) -> Int
selectVal vals pt = (\((_,_),v) -> v) ((filter (\(p,v) -> p == pt) vals) !! 0)

-- return a new updated list with the new value in place
setVal :: [SValue] -> (Int,Int) -> Int -> [SValue]
setVal (((r,c),v):rs) (r1,c1) newVal | and [r==r1,c==c1] = ((r,c),newVal):rs
setVal (val:rs) pt newVal = val:(setVal rs pt newVal)

-- return all of the options for the given point on the sudoku board
--
-- note: options is never called on a point with a number already in it
options :: [SValue] -> (Int,Int) -> [Int]
options vals pt = options' vals pt [1..9]
                  where
                    options' [] _ left = left
                    options' ((newpt,v):vn) pt left = 
                      if pointInRange newpt pt then options' vn pt (delete v left) else options' vn pt left

solve :: SBoard -> Maybe SBoard
solve board = case solve' (assocs board) (0,0) of
  Just vals -> Just (array ((0,0),(8,8)) vals)
  _ -> Nothing

solve' :: [SValue] -> (Int,Int) -> Maybe [SValue]
solve' vals pt = case advance vals pt of
  Just newPt -> fit vals (options vals newPt) newPt
  _ -> Just vals

fit :: [SValue] -> [Int] -> (Int,Int) -> Maybe [SValue]
fit _ [] _ = Nothing
fit vals (fstopt:rstopt) pos = case advance vals pos of
  Just newPt -> case solve' (setVal vals pos fstopt) newPt of
    Just board -> Just board
    Nothing -> fit vals rstopt pos
  _ -> Just vals

-- process command line arguments and get the filename
getSudokuFile [] = putStrLn "Usage: ./SudokuSolver <filename>" >> exit
getSudokuFile [filename] = readFile filename

exit = exitWith ExitSuccess
failure = exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= getSudokuFile >>= (\f -> case SudokuReader.readSudokuFile f of 
                                         Just board -> case solve board of 
                                           Just answer -> (putStrLn $ sudokuToStr answer) >> exit
                                           _ -> (putStrLn "No solution") >> failure
                                         _ -> (putStrLn "Not a valid sudoku board") >> failure)
