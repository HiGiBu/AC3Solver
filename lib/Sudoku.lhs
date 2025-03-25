\begin{code}
module Sudoku where

-- General imports
import Data.List ( intercalate )
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Text.Printf ( printf )

-- Import AC3 solver and backtracking algorithm
import AC3Solver ( AC3 (..), ac3, ConstraintAA, Domain )
import Backtracking ( findSolution )

\end{code}
\subsection{The Sudoku Library}\label{sec:sudoku}

This file implements Sudoku in a suitable format for our AC3 and backtracking algorithms.

In our formulation:

1. Each cell on the Sudoku board is represented as an \textit{Agent} with its associated domain
   - An agent is identified by a coordinate $(i,j)$ where $i$ is the row $[1-9]$ and $j$ is the column $[1-9]$
   - Each agent maintains a domain of possible values $[1-9]$

2. Sudoku's rules are encoded as binary constraints between agents:
\begin{itemize}
    \item \textbf{Row constraint}: All cells in the same row must contain different values
    \item \textbf{Column constraint}: All cells in the same column must contain different values
    \item \textbf{Box constraint}: All cells in the same 3-by-3 box must contain different values
\end{itemize}
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
startingCellsUnique = [ ((1,3), [1]),
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

sudokuExampleDomainUnique :: [Domain (Int, Int) Int]   -- | Complete the initial grid by adding empty cells
sudokuExampleDomainUnique = startingCellsUnique ++ [(i, [1..9]) | i <- allCells, i `notElem` map fst startingCellsUnique]

sudokuExampleUnique :: AC3 (Int, Int) Int
sudokuExampleUnique = AC3 sudokuConstraints sudokuExampleDomainUnique

\end{code}

Instead of specifying our own sudoku puzzles we can leverage the repository of \cite{ashing_jabenjysudoku}, in which 100+ puzzles are available. These puzzles are stored
as nine rows separated by a newline character, each row containing nine entries. Empty cells are represented by ".".

\begin{code}
readSudokuFromFile :: FilePath -> IO [Domain (Int, Int) Int]
readSudokuFromFile filePath = do                -- | filePath is the path to the sudoku puzzle file
    contents <- readFile filePath
    let rows = lines contents
    return (parseSudokuDomains rows)

parseSudokuDomains :: [String] -> [Domain (Int, Int) Int]
parseSudokuDomains rows = cellDomains where
    charToDomain :: Char -> [Int]               -- | Converts characters to domain values
    charToDomain '.' = [1..9]                   -- | Empty cell
    charToDomain c = if c >= '1' && c <= '9'    -- | Should always be true if c!='.', added for safety
                        then [read [c]]         -- | Fixed cell
                        else [1..9]             -- | Empty cell
    
    cellDomains = [((i, j), charToDomain c) |
                  (i, row) <- zip [1..9] (take 9 rows),
                  (j, c) <- zip [1..9] (take 9 row)]

\end{code}

Leveraging these functions we can load a sudoku puzzle by specifying its name and return an AC3 instance.

\begin{code}
loadSudokuPuzzle :: String -> IO (AC3 (Int, Int) Int)
loadSudokuPuzzle fileName = do                  -- | fileName is the name of the sudoku puzzle
    let filePath = "sudokuPuzzles/" ++ fileName ++ ".sud"
    cellDomains <- readSudokuFromFile filePath
    return (AC3 sudokuConstraints cellDomains)
\end{code}

With the tools above, we can finally define a few different functions that the user can interact with. To start with, we
need a function that loads a sudoku puzzle from its file name, runs AC3, and returns the puzzle with its reduced domains.

\begin{code}
runAC3OnSudokuFile :: String -> IO (AC3 (Int, Int) Int)
runAC3OnSudokuFile fileName = do
    puzzle <- loadSudokuPuzzle fileName         -- | Load sudoku puzzle from file name         
    putStrLn "Initial puzzle:"
    printSudokuPuzzle puzzle                    -- | Display the initial puzzle

    putStrLn "Running AC3..."                   -- | Run AC3 and create a new puzzle with reduced domains
    let reducedDomain = ac3 puzzle
    let reducedPuzzle = AC3 sudokuConstraints reducedDomain

    let oldDomain = getDomains puzzle           -- | Display the average domain size before and after running AC3
    let newDomain = getDomains reducedPuzzle
    let (beforeAC3, afterAC3) = computeDomainReduction oldDomain newDomain
    putStrLn "Average domain size"
    putStrLn $ "    Before AC3: " ++ beforeAC3
    putStrLn $ "    After AC3:  " ++ afterAC3

    return reducedPuzzle
\end{code}

The reduction in domain size from running AC3 varies between puzzles, easier puzzles experience greater reduction than
harder puzzles. However, only getting the domain reduction is unsatisfactory, we want a solution to the sudoku as well. 
The following function does just that by running the backtracking algorithm over the AC3 reduced puzzle.

\begin{code}
solveSudokuFromFile :: String -> IO ()
solveSudokuFromFile fileName = do
    reducedPuzzle <- runAC3OnSudokuFile fileName-- | Get the puzzle with reduced domains

    putStrLn "Running backtracking..."          -- | Run backtracking to find a solution
    let solutions = findSolution reducedPuzzle

    let solvedDomain = case solutions of        -- | Check for solutions, extract solved domain if found
                  Nothing -> []                 -- | No solution found
                  Just assignments -> [((row, col), [number]) | ((row, col), number) <- assignments]
    
    if null solvedDomain 
        then putStrLn "No solution was found"
        else do
            let solvedPuzzle = AC3 sudokuConstraints solvedDomain
            putStrLn "Solved puzzle:"
            printSudokuPuzzle solvedPuzzle
\end{code}

With these function we can define the main loop that the user interacts with. It asks the user to choose a sudoku puzzle, and then runs AC3 and backtracking on it.
The user can choose between easy, hard, and special puzzles. Easy and hard puzzles are chosen by number, while special puzzles are chosen by name. Each of these
three cases are considered, and the user is prompted to choose again if an invalid choice is made.

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

        -- | Easy puzzle case
        "1" -> do getEasyPuzzle where -- | Start the recursive prompt
            getEasyPuzzle = do
                putStr "Choose a puzzle number between 1 and 50: "
                puzzleNum <- getLine
                -- Check if input is a valid number in range
                case reads puzzleNum :: [(Int, String)] of
                    [(num, "")] | num >= 1 && num <= 50 -> 
                        return ("easy" ++ puzzleNum)
                    _ -> do
                        putStrLn $ "Invalid choice. Please enter a number between 1 and 50."
                        getEasyPuzzle -- | Try again

        -- | Hard puzzle case
        "2" -> do getHardPuzzle where -- | Start the recursive prompt
            getHardPuzzle = do
                    putStr "Choose a puzzle number between 1 and 95: "
                    puzzleNum <- getLine
                    -- Check if input is a valid number in range
                    case reads puzzleNum :: [(Int, String)] of
                        [(num, "")] | num >= 1 && num <= 95 -> 
                            return ("hard" ++ puzzleNum)
                        _ -> do
                            putStrLn $ "Invalid choice. Please enter a number between 1 and 95."
                            getHardPuzzle -- | Try again
        
        -- | Special puzzle case
        "3" -> do askForSpecialPuzzle where -- | Start the recursive prompt
            askForSpecialPuzzle = do
                putStr "Choose a puzzle: \n\
                    \  (1) impossible\n\
                    \  (2) Mirror\n\
                    \  (3) Times1\n"
                
                putStr "\nSelect one of (1, 2, 3): "
                puzzleName <- getLine
                case puzzleName of
                    "1" -> return "impossible"
                    "2" -> return "Mirror"
                    "3" -> return "Times1"
                    _   -> do
                        putStrLn $ "Sorry, " ++ show puzzleName ++ " is not a valid choice. Please try again."
                        askForSpecialPuzzle -- | Try again
        
        -- | Invalid choice
        x -> do
            putStrLn $ "Sorry, " ++ show x ++ " is not a valid choice. Please try again."
            sudokuMain  -- | Restart if invalid choice
            return ""   -- | This line is dealt with below

    if null fileName then return () -- | fileName is null after user executes case x, but the program has already successfully run
    else do
        -- | Solve the Sudoku puzzle from the file
        putStrLn $ "\nSolving Sudoku puzzle " ++ fileName ++ "..."
        solveSudokuFromFile fileName

-- | Display welcome banner
showWelcomeMessage :: IO ()
showWelcomeMessage = do
    putStrLn "------------------------------------------" 
    putStrLn "|            WELCOME TO THE              |"
    putStrLn "|           SUDOKU AC3 SOLVER            |"
    putStrLn "------------------------------------------"
    putStrLn ""

\end{code}










\hide{

The code above relied on some pretty-printing and domain size reduction, which are defined below.

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

computeDomainReduction :: [[Int]] -> [[Int]] -> (String, String)
computeDomainReduction oldDomain newDomain = (printf "%.2f" oldSizeAverage, printf "%.2f" newSizeAverage)
  where
    oldSize = sum (map length oldDomain)
    newSize = sum (map length newDomain)
    oldSizeAverage = fromIntegral oldSize / 81 :: Float
    newSizeAverage = fromIntegral newSize / 81 :: Float

-- Extract the domains of each cell from a sudoku puzzle
getDomains :: AC3 (Int, Int) Int -> [[Int]]
getDomains (AC3 _cons dom) = map snd dom

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


\begin{code}
solveSudokuFromFileSilent :: String -> IO ()
solveSudokuFromFileSilent fileName = do
    puzzle <- loadSudokuPuzzle fileName    
    start <- getCurrentTime         -- | Start timer     
    let ac3Domain = ac3 puzzle
    let ac3Puzzle = AC3 sudokuConstraints ac3Domain
    let solutions = findSolution ac3Puzzle
    let solvedDomain = case solutions of
                  Nothing -> []  -- No solution found
                  Just assignments -> [((row, col), [number]) | ((row, col), number) <- assignments]
    if null solvedDomain then putStrLn "No solution found"
    else do
        end <- getCurrentTime       -- | End timer
        putStrLn $ "Solved puzzle: " ++ show fileName ++ " in " ++ show (diffUTCTime end start)

-- | Silent version that runs AC3 on a file
runAC3OnSudokuFileSilent :: String -> IO (String, String, [[Int]], [[Int]])
runAC3OnSudokuFileSilent fileName = do
    puzzle <- loadSudokuPuzzle fileName         -- | Load sudoku puzzle from file name         
    -- putStrLn "Initial puzzle:"
    -- printSudokuPuzzle puzzle                    -- | Display the initial puzzle

    -- putStrLn "Running AC3..."                   -- | Run AC3 and create a new puzzle with reduced domains
    let reducedDomain = ac3 puzzle
    let reducedPuzzle = AC3 sudokuConstraints reducedDomain

    let oldDomain = getDomains puzzle           -- | Display the average domain size before and after running AC3
    let newDomain = getDomains reducedPuzzle
    let (beforeAC3, afterAC3) = computeDomainReduction oldDomain newDomain
    
    -- putStrLn "Average domain size"
    -- putStrLn $ "    Before AC3: " ++ beforeAC3
    -- putStrLn $ "    After AC3:  " ++ afterAC3

    -- | Return the old and new domains instead
    return (beforeAC3, afterAC3, oldDomain, newDomain)


solveSudokuFromFileNOAC3 :: String -> IO ()
solveSudokuFromFileNOAC3 fileName = do
    puzzle <- loadSudokuPuzzle fileName       
    putStrLn "Initial puzzle"
    printSudokuPuzzle puzzle

    putStrLn "Running backtracking..."          -- | Run backtracking to find a solution
    let solutions = findSolution puzzle

    let solvedDomain = case solutions of        -- | Check for solutions, extract solved domain if found
                  Nothing -> []                 -- | No solution found
                  Just assignments -> [((row, col), [number]) | ((row, col), number) <- assignments]
    
    if null solvedDomain 
        then putStrLn "No solution was found"
        else do
            let solvedPuzzle = AC3 sudokuConstraints solvedDomain
            putStrLn "Solved puzzle:"
            printSudokuPuzzle solvedPuzzle

-- | Solving a few easy puzzles with AC3 and measuring the time
easyFiles' :: [[Char]]
easyFiles' = ["easy" ++ show i | i <- [1, 43, 32, 12, 44, 3, 22, 13, 34, 45:: Integer]]

solvingEasyPuzzlesExperiment :: IO [()]
solvingEasyPuzzlesExperiment = do
    mapM solveSudokuFromFileSilent easyFiles'
    
hardFiles' :: [[Char]]
hardFiles' = ["hard" ++ show i | i <- [75, 92, 38, 34, 64, 78, 96, 56, 53, 12:: Integer]]


\end{code}

This code was used to conduct the domain reduction experiment.

\begin{code}

easyFiles :: [[Char]]
easyFiles = ["easy" ++ show i | i <- [1..50 :: Integer]]

hardFiles :: [[Char]]
hardFiles = ["hard" ++ show i | i <- [1..95 :: Integer]]

domainReductionExperimentEasy :: IO ()
domainReductionExperimentEasy = do
    results <- mapM runAC3OnSudokuFileSilent easyFiles

    -- | Extract the old and new (before and after AC3) average domain sizes
    let oldAvgDomains = map (\(old, _, _, _) -> read old :: Float) results
    let newAvgDomains = map (\(_, new, _, _) -> read new :: Float) results

    print oldAvgDomains
    print newAvgDomains

domainReductionExperimentHard :: IO ()
domainReductionExperimentHard = do
    results <- mapM runAC3OnSudokuFileSilent hardFiles

    -- | Extract the old and new (before and after AC3) average domain sizes
    let oldAvgDomains = map (\(old, _, _, _) -> read old :: Float) results
    let newAvgDomains = map (\(_, new, _, _) -> read new :: Float) results

    print oldAvgDomains
    print newAvgDomains

domainCombinationsExperiment :: String -> IO ()
domainCombinationsExperiment fileName = do
    (_, _, oldDomains, newDomains) <- runAC3OnSudokuFileSilent fileName
    
    putStrLn "Old domains number of combinations:"
    printNumberofCombinations oldDomains
    putStrLn "New domains number of combinations:"
    printNumberofCombinations newDomains
    where
        printNumberofCombinations domains' = do
            let a = map length domains'
            let logProduct = sum $ map (logBase 10 . fromIntegral) a :: Float
            let flooredValue = floor logProduct :: Int
            putStrLn $ "Total combinations: approximately 10^" ++ show flooredValue

runDomainCombinationsExperiment :: IO ()
runDomainCombinationsExperiment = do
    mapM_ domainCombinationsExperiment easyFiles
    -- mapM_ domainCombinationsExperiment hardFiles

\end{code}


}
