CC=ghc

all: SudokuSolver

SudokuSolver: SudokuSolver.hs
	$(CC) -main-is SudokuSolver SudokuSolver.hs
	rm *.hi *.o
