-- compile with -main-is SudokuSolver to get to work with GHC

module SudokuSolver where
import System.Environment
import System.Exit
import Data.Char
import Data.Array
-- import Data.Array.IArray

-- type SBoard = IArray (IOToDiffArray IOUArray) Int
type SValue = ((Int, Int), Int)
type SBoard = Array (Int, Int) Int

-- true if the character should be ignored
ignoreCharPred :: Char -> Bool
ignoreCharPred c = or [c == ' ', c == '\n', c == '\r', c == '|', c == '-', c == '+']

-- determine if the current entry is valid
entryPred :: (Int, Int) -> Char -> Bool
entryPred (r,c) v = and [r < 9, r >= 0, c < 9, c >= 0, val < 10, val >= 0]
                    where
                      val = digitToInt v

-- reurn the current entry
getEntry :: (Int, Int) -> Char -> Maybe SValue
getEntry coord v | entryPred coord v = Just (coord, digitToInt v)
getEntry _ _ = Nothing

-- helper function: builds up the list in an accumulator
--
-- skips any whitespace characters
getSudokuVals' :: [Char] -> (Int, Int) -> Maybe [SValue] -> Maybe [SValue]
getSudokuVals' [] _ acc = acc
getSudokuVals' (v:vn) coord acc | ignoreCharPred v = getSudokuVals' vn coord acc
getSudokuVals' (v:vn) (r,c) acc = getSudokuVals' vn (nextR, nextC) lst
                                  where
                                    nextC = (c+1) `mod` 9
                                    nextR = if nextC == 0 then r+1 else r
                                    lst = case acc of
                                      Just vals -> case getEntry (r,c) v of
                                        Just entry -> Just (entry:vals)
                                        _ -> Nothing
                                      _ -> Nothing

-- wrapper around the call to the helper function
getSudokuVals :: [Char] -> Maybe [SValue]
getSudokuVals s = getSudokuVals' s (0,0) (Just [])

-- officially wrap it in an array
readSudokuFile :: String -> Maybe SBoard
readSudokuFile s = case getSudokuVals s of 
                           Just vals -> Just (array ((0,0),(8,8)) vals)
                           _ -> Nothing

main :: IO ()
main = getArgs >>= getSudokuFile >>= (\f -> case readSudokuFile f of 
                                         Just board -> (putStrLn $ show board) >> exit
                                         _ -> (putStrLn "not a valid sudoku board") >> failure)

-- process command line arguments and get the filename
getSudokuFile [] = putStrLn "Usage: ./SudokuSolver <filename>" >> exit
getSudokuFile [filename] = readFile filename

exit = exitWith ExitSuccess
failure = exitWith (ExitFailure 1)