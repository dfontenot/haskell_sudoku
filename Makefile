CC=ghc
MAINIS=SudokuSolver
TESTMAINIS=SudokuTests

all: SudokuSolver

clean: SudokuSolver SudokuTests
	rm SudokuSolver SudokuTests

SudokuSolver: SudokuSolver.hs SudokuReader.hs
	$(CC) -main-is $(MAINIS) SudokuSolver.hs SudokuReader.hs
	rm *.hi *.o

tests: SudokuSolver.hs SudokuReader.hs SudokuTests.hs
	$(CC) -main-is $(TESTMAINIS) SudokuTests.hs SudokuReader.hs SudokuSolver.hs
	rm *.hi *.o
	./SudokuTests
