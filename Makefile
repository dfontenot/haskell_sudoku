CC=ghc

all: SudokuSolver

SudokuSolver: SudokuSolver.hs SudokuReader.hs
	$(CC) -main-is SudokuSolver SudokuSolver.hs SudokuReader.hs
	rm *.hi *.o
