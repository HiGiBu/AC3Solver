\begin{code}
module Sudoku where

-- General imports
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Import AC3 solver and backtracking algorithm
import AC3Solver ( AC3 (..), ac3, ConstraintAA, Domain )
import Backtracking ( findSolution )
\end{code}

This file implements Sudoku in a suitable format for our AC3 and backtracking algorithms.

In our formulation:

1. Each cell on the Sudoku board is represented as an \textit{Agent} with its associated domain
   - An agent is identified by a coordinate $(i,j)$ where $i$ is the row $[1-9]$ and $j$ is the column $[1-9]$
   - Each agent maintains a domain of possible values $[1-9]$

2. Sudoku's rules are encoded as binary constraints between agents:
   - \textbf{Row constraint}: All cells in the same row must contain different values
   - \textbf{Column constraint}: All cells in the same column must contain different values
   - \textbf{Box constraint}: All cells in the same 3-by-3 box must contain different values

These constraints are implemented as inequality relations ($\neq$) between cells. For instance, cell $(3,2)$ 
and cell $(3,7)$ are in the same row, thus, a constraint is added to ensure that they do not have the same value.

Below is the definition for a list of all cells, and the conditions for two cells being on the same row and
in the same column.

\begin{code}
allCells :: [(Int, Int)]
allCells = [(i,j) | i <- [1..9], j <- [1..9]]

sameRow :: (Int, Int) -> (Int, Int) -> Bool
sameRow (x1,_) (y1,_) = x1 == y1

sameCol :: (Int, Int) -> (Int, Int) -> Bool
sameCol (_,x2) (_,y2) = x2 == y2
\end{code}

A similar condition can be constructed for two cells being in the same 3-by-3 box.

\begin{code}
sameBox :: (Int, Int) -> (Int, Int) -> Bool
sameBox (x1,y1) (x2,y2) = (x1 - 1) `div` 3 == (x2 - 1) `div` 3 && (y1 - 1) `div` 3 == (y2 - 1) `div` 3
\end{code}

With these conditions, the constraints can be modelled as a list of inequalities between cells. Two cells receive
an inequality if they are distinct and share the same row, column, or box.

\begin{code}
sudokuConstraints :: [ConstraintAA (Int, Int) Int]
sudokuConstraints =    [(i, j, (/=)) | i <- allCells, j <- allCells, i /= j, i `sameRow` j]
                    ++ [(i, j, (/=)) | i <- allCells, j <- allCells, i /= j, i `sameCol` j]
                    ++ [(i, j, (/=)) | i <- allCells, j <- allCells, i /= j, i `sameBox` j]

\end{code}

Below is an example of an empty Sudoku board, that is, the domain of every cell is $[1..9]$. 
    
\begin{code}
sudokuDomains :: [Domain (Int, Int) Int]
sudokuDomains = [(i, [1..9]) | i <- allCells]

sudokuEmpty :: AC3 (Int, Int) Int
sudokuEmpty = AC3 sudokuConstraints sudokuDomains
\end{code}

Using the `sudokuConstraints` as a backbone we can define our own sudoku puzzle. It is quite tedious because it 
requires us to specify the initial grid. The example below is a Sudoku puzzle with a unique solution.

\begin{code}
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

sudokuExampleDomainUnique :: [Domain (Int, Int) Int]   -- Complete the initial grid by adding empty cells
sudokuExampleDomainUnique = startingCellsUnique ++ [(i, [1..9]) | i <- allCells, i `notElem` map fst startingCellsUnique]

sudokuExampleUnique :: AC3 (Int, Int) Int
sudokuExampleUnique = AC3 sudokuConstraints sudokuExampleDomainUnique

\end{code}

Instead of specifying our own sudoku puzzles we can leverage the repository of \cite{ashing_jabenjysudoku}, in which 100+ puzzles are available. These puzzles are stored
as nine rows separated by a newline character, each row containing nine entries. Empty cells are represented by ".".

\begin{code}
readSudokuFromFile :: FilePath -> IO [Domain (Int, Int) Int]
readSudokuFromFile filePath = do                -- filePath is the path to the sudoku puzzle file
    contents <- readFile filePath
    let rows = lines contents
    return (parseSudokuDomains rows)

parseSudokuDomains :: [String] -> [Domain (Int, Int) Int]
parseSudokuDomains rows = cellDomains where
    charToDomain :: Char -> [Int]               -- Converts characters to domain values
    charToDomain '.' = [1..9]                   -- Empty cell
    charToDomain c = if c >= '1' && c <= '9'    -- Should always be true if c!='.', added for safety
                        then [read [c]]         -- Fixed cell
                        else [1..9]             -- Empty cell
    -- A list of agents and their domains
    cellDomains = [((i, j), charToDomain c) |
                  (i, row) <- zip [1..9] (take 9 rows),
                  (j, c) <- zip [1..9] (take 9 row)]

\end{code}

Putting these functions to use we can load a sudoku puzzle from its name and return an AC3 instance.

\begin{code}
loadSudokuPuzzle :: String -> IO (AC3 (Int, Int) Int)
loadSudokuPuzzle fileName = do                  -- fileName is the name of the sudoku puzzle
    let filePath = "sudokuPuzzles/" ++ fileName ++ ".sud"
    cellDomains <- readSudokuFromFile filePath
    return (AC3 sudokuConstraints cellDomains)
\end{code}

Below are two functions that take a file (name) as input and does:
- `solveSudokuFromFile`: loads a sudoku puzzle from a file, runs AC3, and prints the initial and final state of the puzzle.
- `computeReductionFromFile`: loads a sudoku puzzle from a file, runs AC3, and computes the average domain size before and after running AC3.

Note the input to both of the above should be a \textbf{file name}, which means one of the following:
- "easy1", "easy2", ..., "easy50",
- "hard1", "hard2", ..., "hard95",
- "impossible", "Mirror", "Times1".

Functionalities should be:
- Load sudoku file, display it, run AC3, display the time it took and the reduction in average domain size.
- Run the above, and take the output and run backtracking get an actual solution, display the time it took and the solution.
- Load sudoku file, display it, but only run backtracking, display time.

\begin{code}
-- Load, run AC3, and print a Sudoku puzzle
solveSudokuFromFile :: String -> IO ()
solveSudokuFromFile fileName = do
    puzzle <- loadSudokuPuzzle fileName         
    putStrLn "Initial puzzle:"
    printSudokuPuzzle puzzle

    -- Run AC3 and create a new puzzle with reduced domains
    putStrLn "Running AC3..."
    let ac3Domain = ac3 puzzle
    let ac3Puzzle = AC3 sudokuConstraints ac3Domain

    putStrLn "Running backtracking..."
    let solutions = findSolution ac3Puzzle

    let solvedDomain = case solutions of
                  Nothing -> []  -- No solution found
                  Just assignments -> [((row, col), [number]) | ((row, col), number) <- assignments]
    
    if null solvedDomain then putStrLn "No solution found"
    else do
        let solvedPuzzle = AC3 sudokuConstraints solvedDomain
        putStrLn "Solved puzzle:"
        printSudokuPuzzle solvedPuzzle

solveSudokuFromFileSilent :: String -> IO ()
solveSudokuFromFileSilent fileName = do
    puzzle <- loadSudokuPuzzle fileName         
    let ac3Domain = ac3 puzzle
    let ac3Puzzle = AC3 sudokuConstraints ac3Domain
    let solutions = findSolution ac3Puzzle
    let solvedDomain = case solutions of
                  Nothing -> []  -- No solution found
                  Just assignments -> [((row, col), [number]) | ((row, col), number) <- assignments]
    
    if null solvedDomain then putStrLn "No solution found"
    else do
        putStrLn "Solved puzzle!"



solveSudokuFromFileNOAC3 :: String -> IO ()
solveSudokuFromFileNOAC3 fileName = do
    puzzle <- loadSudokuPuzzle fileName         
    let _solutions = findSolution puzzle
    return ()



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



\begin{code}

sudokuMain :: IO ()
sudokuMain = do
    showWelcomeMessage

    putStr "Choose your difficulty: \n\
         \  (1) easy\n\
         \  (2) hard\n\
         \  (3) special\n"
    
    putStr "\nSelect one of (1, 2, 3): "
    diff <- getLine

    fileName <- case diff of
        "1" -> do
            putStr "Choose a puzzle number between 1 and 50: "
            puzzleNum <- getLine
            return ("easy" ++ puzzleNum)
            
        "2" -> do
            putStr "Choose a puzzle number between 1 and 95: "
            puzzleNum <- getLine
            return ("hard" ++ puzzleNum)
            
        "3" -> do
            putStr "Choose a puzzle: \n\
                \  (1) impossible\n\
                \  (2) Mirror\n\
                \  (3) Times1\n"
            putStr "\nYour choice: "
            puzzleName <- getLine
            return $ case puzzleName of
                "1" -> "impossible"
                "2" -> "Mirror"
                "3" -> "Times1"
                _   -> "impossible"
                
        _ -> do
            putStrLn "Invalid choice. Please try again."
            sudokuMain  -- Restart if invalid choice
            return ""   -- This line is never reached but needed for type checking

    -- Print the initial puzzle and the result after applying AC3
    putStr $ "\nSolving Sudoku puzzle " ++ fileName ++ "...\n"

    (beforeAC3, afterAC3) <- computeReductionFromFile fileName
    putStrLn "The average domain size:"
    putStrLn $ "    Before AC3: " ++ show beforeAC3
    putStrLn $ "    After AC3: " ++ show afterAC3

    -- Solve the Sudoku puzzle from the file
    solveSudokuFromFile fileName
    
-- Display welcome banner
showWelcomeMessage :: IO ()
showWelcomeMessage = do
    putStrLn "------------------------------------------" 
    putStrLn "|           SUDOKU AC3 SOLVER            |"
    putStrLn "------------------------------------------"
    putStrLn ""


\end{code}



Benchmarking the speed of solving sudoku puzzles with AC3 + backtracking VS. only backtracking.

\begin{code}

timeAC3 :: String -> IO ()
timeAC3 fileName = do
    start <- getCurrentTime
    solveSudokuFromFile fileName
    end <- getCurrentTime
    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

\end{code}