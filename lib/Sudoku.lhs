\begin{code}
module Sudoku where

import Data.List (intercalate)
import AC3Solver ( AC3 (..), ac3, ConstraintAA, Domain )

\end{code}

This file contains the implementation of Sudoku puzzles, printing Sudoku boards, and calling the AC3 solver on puzzles.

The constraints on any given cell are:
    1. it must contain a distinct number from other cells in the same row,
    2. it must contain a distinct number from other cells in the same column,
    3. it must contain a distinct number from other cells in the same 3x3 box. 

An \textit{Agent} is represented by a cell, which in-turn is represented as a tuple $(i,j)$ where $i$ is the row number and $j$ is the column number. 

\begin{code}
-- Encode all possible cells in a 9x9 grid
allCells :: [(Int, Int)]
allCells = [(i,j) | i <- [1..9], j <- [1..9]]

sameRow :: (Int, Int) -> (Int, Int) -> Bool
sameRow (x1,_) (y1,_) = x1 == y1

sameCol :: (Int, Int) -> (Int, Int) -> Bool
sameCol (_,x2) (_,y2) = x2 == y2

sameBox :: (Int, Int) -> (Int, Int) -> Bool
sameBox (x1,y1) (x2,y2) = (x1 - 1) `div` 3 == (x2 - 1) `div` 3 && (y1 - 1) `div` 3 == (y2 - 1) `div` 3
-- for all x: (fst x - 1) in {0,1,2} (box row)
-- for all y: (snd y - 1) in {0,1,2} (box column)
-- when both the box row and box column are the same for x and y, they are in the same box.

-- All cells must obey the three constraints.
sudokuConstraints :: [ConstraintAA (Int, Int) Int]
sudokuConstraints =    [(i, j, (/=)) | i <- allCells, j <- allCells, i /= j, i `sameRow` j]
                    ++ [(i, j, (/=)) | i <- allCells, j <- allCells, i /= j, i `sameCol` j]
                    ++ [(i, j, (/=)) | i <- allCells, j <- allCells, i /= j, i `sameBox` j]

-- The domain will change depending on the puzzle being solved. Some cells should have fixed values.
-- Each cell can take on a value from 1 to 9.
sudokuDomains :: [Domain (Int, Int) Int]
sudokuDomains = [(i, [1..9]) | i <- allCells]

-- An empty Sudoku puzzle (no cells filled in)
sudokuMain :: IO () --AC3 (Int, Int) Int
sudokuMain = undefined--AC3 sudokuConstraints sudokuDomains

\end{code}

Using the `sudokuConstraints` as a backbone we can define our own sudoku puzzle. It's quite tedious because it requires us to specify the initial grid. 
The particular example below is a Sudoku puzzle with a unique solution.

\begin{code}

-- Define the initial grid
startingCellsUnique :: [Domain (Int, Int) Int]
startingCellsUnique = [
    ((1,3), [1]),
    ((1,5), [6]),
    ((1,9), [4]),
    ((2,1), [8]),
    ((2,4), [1]),
    ((2,6), [4]),
    ((2,8), [6]),
    ((3,2), [3]),
    ((3,7), [8]),
    ((3,9), [5]),
    ((4,1), [7]),
    ((4,3), [8]),
    ((4,5), [2]),
    ((4,9), [3]),
    ((5,2), [6]),
    ((5,3), [3]),
    ((5,7), [1]),
    ((5,8), [2]),
    ((5,9), [9]),
    ((6,5), [1]),
    ((7,1), [3]),
    ((7,7), [2]),
    ((7,9), [8]),
    ((8,1), [1]),
    ((8,3), [4]),
    ((8,5), [5]),
    ((8,6), [9]),
    ((8,7), [3]),
    ((9,2), [7]),
    ((9,4), [8]),
    ((9,6), [3]),
    ((9,7), [5]),
    ((9,8), [9]),
    ((9,9), [1])
    ]

-- Combine the starting values with the rest of the (empty) cells
sudokuExampleDomainUnique :: [Domain (Int, Int) Int]
sudokuExampleDomainUnique = startingCellsUnique ++ [(i, [1..9]) | i <- allCells, i `notElem` map fst startingCellsUnique]

sudokuExampleUnique :: AC3 (Int, Int) Int
sudokuExampleUnique = AC3 sudokuConstraints sudokuExampleDomainUnique

\end{code}

Instead of specifying our own sudoku puzzles we can leverage a repository of sudoku puzzles. Below is the code
needed to load sudoku puzzles.

\begin{code}

-- Read a Sudoku puzzle from a file
readSudokuFromFile :: FilePath -> IO [Domain (Int, Int) Int]
readSudokuFromFile filepath = do
    contents <- readFile filepath
    let rows = lines contents
    return (parseSudokuDomains rows)


-- Parse the file contents into domain representation
parseSudokuDomains :: [String] -> [Domain (Int, Int) Int]
parseSudokuDomains rows = cellDomains where
    -- Convert characters to domain values
    charToDomain :: Char -> [Int]
    -- Dots represent empty cells, their domain contains all possible values
    charToDomain '.' = [1..9]
    -- Digits represent fixed cells, their domain contains only that value
    charToDomain c = if c >= '1' && c <= '9' then [read [c]] else [1..9]
    
    -- Return a list of domains for each cell
    cellDomains = [((i, j), charToDomain c) |
                  (i, row) <- zip [1..9] (take 9 rows), -- rows :: [String] where each string is a row
                  (j, c) <- zip [1..9] (take 9 row)]


-- Create a AC3 formatted Sudoku puzzle from a file
loadSudokuPuzzle :: String -> IO (AC3 (Int, Int) Int)
loadSudokuPuzzle fileName = do
    let filepath = "sudokuPuzzles/" ++ fileName ++ ".sud"
    cellDomains <- readSudokuFromFile filepath
    return (AC3 sudokuConstraints cellDomains)

\end{code}

Below are two functions that take a file (name) as input and does:
- `solveSudokuFromFile`: loads a sudoku puzzle from a file, runs AC3, and prints the initial and final state of the puzzle.
- `computeReductionFromFile`: loads a sudoku puzzle from a file, runs AC3, and computes the average domain size before and after running AC3.

Note the input to both of the above should be a \textbf{file name}, which means one of the following:
- "easy1", "easy2", ..., "easy50",
- "hard1", "hard2", ..., "hard95",
- "impossible", "Mirror", "Times1".

\begin{code}
-- Load, run AC3, and print a Sudoku puzzle
solveSudokuFromFile :: String -> IO ()
solveSudokuFromFile fileName = do
    puzzle <- loadSudokuPuzzle fileName         
    putStrLn "Initial puzzle:"
    printSudokuPuzzle puzzle
    putStrLn "\nAfter applying AC3:"
    printSudokuSolution puzzle

-- Compute the average domain size before and after running AC3
computeReductionFromFile :: String -> IO (Float, Float)
computeReductionFromFile fileName = do
    puzzle <- loadSudokuPuzzle fileName
    return (computeReduction puzzle)

\end{code}

The code above relied on some pretty-printing and reduction computation, which are defined below.

\begin{code}

-- Visualize a Sudoku board based on current domains
visualizeSudoku :: [Domain (Int, Int) Int] -> String
visualizeSudoku domains' = unlines (
    horizontalLine : concatMap formatRow [1 .. 9]
  )
  where
    -- Get the domain for a specific cell
    getDomain i j = 
        case [d | ((i', j'), d) <- domains', i' == i, j' == j] of
            (d:_) -> d
            [] -> [1..9]  -- Default domain if not specified
    
    -- Display a cell: single digit if domain has 1 element, empty otherwise
    cellValue i j = 
        let domain' = getDomain i j
        in if length domain' == 1 
           then show (head domain')
           else " "
    
    -- Horizontal line patterns
    horizontalLine = "+---+---+---+---+---+---+---+---+---+"
    thickLine = "+===+===+===+===+===+===+===+===+===+"
    
    -- Format a row with appropriate separators
    formatRow i = 
        let rowStr = "| " ++ intercalate " | " [cellValue i j | j <- [1..9]] ++ " |"
            separator = if i `mod` 3 == 0 && i /= 9 then thickLine else horizontalLine
        in [rowStr, separator]
    
-- Function to print the initial state of a Sudoku puzzle
printSudokuPuzzle :: AC3 (Int, Int) Int -> IO ()
printSudokuPuzzle (AC3 _ domains') = do
    putStrLn (visualizeSudoku domains')

-- Function to run and print a Sudoku solution
printSudokuSolution :: AC3 (Int, Int) Int -> IO ()
printSudokuSolution puzzle = do
    let solution = ac3 puzzle
    putStrLn (visualizeSudoku solution)

\end{code}

\begin{code}

-- Compute the amount of reduction in domain size before and after applying AC-3

computeReduction :: AC3 (Int, Int) Int -> (Float, Float)
computeReduction puzzle = (fromIntegral originalSize/81, fromIntegral reducedSize/81)
  where
    originalSize = sum (map length (getDomains puzzle))
    reducedSize = sum (map (length . snd) (ac3 puzzle)) -- Using AC-3 algorithm to reduce the domains

-- Extract the domains of each cell from a sudoku puzzle
getDomains :: AC3 (Int, Int) Int -> [[Int]]
getDomains (AC3 _ domains') = map snd domains'

\end{code}