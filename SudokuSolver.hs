-- compile with -main-is SudokuSolver to get to work with GHC

module SudokuSolver where
import qualified SudokuReader as SR

import System.Environment
import System.Exit

-- process command line arguments and get the filename
getSudokuFile [] = putStrLn "Usage: ./SudokuSolver <filename>" >> exit
getSudokuFile [filename] = readFile filename

exit = exitWith ExitSuccess
failure = exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= getSudokuFile >>= (\f -> case SR.readSudokuFile f of 
                                         Just board -> (putStrLn $ show board) >> exit
                                         _ -> (putStrLn "not a valid sudoku board") >> failure)
