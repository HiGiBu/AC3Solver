\subsection{The Sudoku Library}\label{sec:sudoku}

\begin{code}
module Sudoku where

-- General imports
import Data.List ( intercalate )
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Text.Printf ( printf )
import System.IO (hFlush, stdout)

-- Import AC3 solver and backtracking algorithm
import AC3Solver ( AC3 (..), ac3, Arc, Domain )
import Backtracking ( findSolution )

\end{code}

Sudoku is a popular puzzle that consists of a 9-by-9 grid, divided into 3-by-3 boxes. The goal is to fill the grid with numbers from $1$ to $9$, such that each number appears exactly once in each row, column, and box.
Parts of the implementation are hidden for sake of brevity, but the full code is available in the repository. 
We will begin by defining the Sudoku board and its constraints. \\

In our formulation:
\begin{enumerate}
    \item Each cell on the Sudoku board is represented as a \textit{Variable} with its associated domain. A variable is identified by a coordinate $(i,j)$ where $i$ is the row $[1-9]$ and $j$ is the column $[1-9]$. Each variable maintains a domain of possible values $[1-9]$.
    \item Sudoku's rules are encoded as binary constraints between variables:
    \begin{itemize}
        \item \textbf{Row constraint}: Two cells in the same row must have different values
        \item \textbf{Column constraint}: Two cells in the same column must have different values
        \item \textbf{Box constraint}: Two cells in the same 3-by-3 box must have different values
    \end{itemize}
    These constraints are implemented as inequality relations ($\neq$) between cells. For instance, cell $(3,2)$ 
    and cell $(3,7)$ are in the same row, thus, a constraint is added to ensure that they do not have the same value.
\end{enumerate}

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
sameBox (x1,y1) (x2,y2) =   (x1 - 1) `div` 3 == (x2 - 1) `div` 3 && 
                            (y1 - 1) `div` 3 == (y2 - 1) `div` 3
\end{code}

With these conditions, the constraints can be modelled as a list of inequalities between cells. Two cells receive
an inequality if they are distinct and share the same row, column, or box.

\begin{code}
sudokuConstraints :: [Arc (Int, Int) Int]
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

\hide{
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
}

\subsubsection{Loading Sudoku Puzzles}

As opposed to specifying our own sudoku puzzles, we can leverage the repository of \cite{ashing_jabenjysudoku}, in which 100+ puzzles are available. These puzzles are stored
as nine rows separated by a newline character, each row containing nine entries. Empty cells are represented by ".".

\begin{code}
readSudokuFromFile :: FilePath -> IO [Domain (Int, Int) Int]
readSudokuFromFile filePath = do -- filePath is the path to the sudoku puzzle file
    contents <- readFile filePath
    let rows = lines contents
    return (parseSudokuDomains rows)

-- | Input is list of strings such as ["53..7....", "6..195...", ...]
parseSudokuDomains :: [String] -> [Domain (Int, Int) Int]
parseSudokuDomains rows = cellDomains where
    -- | Assigns a character to its corresponding domain values
    charToDomain :: Char -> [Int]               -- ^ Converts characters to domain values
    charToDomain '.' = [1..9]                   -- ^ Empty cell
    charToDomain c = if c >= '1' && c <= '9'    -- ^ Should always be true if c!='.', included for safety
                        then [read [c]]         -- ^ Value of cell
                        else [1..9]             -- ^ Empty cell

    cellDomains = [((i, j), charToDomain c) |
                  (i, row) <- zip [1..9] (take 9 rows),
                  (j, c) <- zip [1..9] (take 9 row)]

\end{code}

Leveraging these functions we can load a sudoku puzzle by specifying its name and return an AC3 instance.

\begin{code}
loadSudokuPuzzle :: String -> IO (AC3 (Int, Int) Int)
loadSudokuPuzzle fileName = do -- fileName is the name of the sudoku puzzle
    let filePath = "sudokuPuzzles/" ++ fileName ++ ".sud"
    cellDomains <- readSudokuFromFile filePath
    return (AC3 sudokuConstraints cellDomains)
\end{code}

With the tools above, we can finally define a few different functions that the user can interact with. To start with, we
need a function that loads a sudoku puzzle from its file name, runs AC3, and returns the puzzle with its reduced domains.

\begin{code}
-- | Input should be a string such as "easy9"
runAC3OnSudokuFile :: String -> IO (AC3 (Int, Int) Int)
runAC3OnSudokuFile fileName = do
    puzzle <- loadSudokuPuzzle fileName         -- ^ Load sudoku puzzle from file name         
    putStrLn "Initial puzzle:"
    printSudokuPuzzle puzzle                    -- ^ Display the initial puzzle

    putStrLn "Running AC3..."                   -- ^ Run AC3 and create a new puzzle with reduced domains
    let reducedDomain = ac3 puzzle
    let reducedPuzzle = AC3 sudokuConstraints reducedDomain

    let oldDomain = getDomains puzzle           -- ^ Display the average domain size before and after running AC3
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
-- | Input should be a string such as "easy9"
solveSudokuFromFile :: String -> IO ()
solveSudokuFromFile fileName = do
    reducedPuzzle <- runAC3OnSudokuFile fileName-- ^ Get the puzzle with reduced domains

    putStrLn "Running backtracking..."          -- ^ Run backtracking to find a solution
    let solutions = findSolution reducedPuzzle

    let solvedDomain = case solutions of        -- ^ Check for solutions, extract solved domain if found
                  Nothing -> []                 -- ^ No solution found
                  Just assignments -> [((row, col), [number]) | ((row, col), number) <- assignments]
    
    if null solvedDomain 
        then putStrLn "No solution was found"
        else do
            let solvedPuzzle = AC3 sudokuConstraints solvedDomain
            putStrLn "Solved puzzle:"
            printSudokuPuzzle solvedPuzzle
\end{code}

\subsubsection{Benchmarking AC3 for Sudoku}

We could not include a benchmark for solving Sudoku puzzles with and without AC3, as it took too long without it. When we tried to run backtracking on puzzles without first reducing the domains using AC3, it could not solve easy Sudoku puzzles within an hour. As seen in table \ref{tab:ac3_comparison}, before applying AC3, there are on average $10^{49}$ number of board configurations for an easy Sudoku puzzle, and $10^{57}$ for a hard puzzle. As backtracking makes random guesses in this vast search space, it becomes unpractical. However, when paired with AC3 it can solve both easy and hard puzzles within minutes. \\

AC3 turns out to be an effective way to significantly reduce the domain size of Sudoku puzzles. It never took more than a couple of seconds to run, and consistently removes 3-4 alternatives from each cell on average, as seen in figure \ref{fig:domainSizeReduction}. Notably, AC3 manages to \textit{solve} a handful of easy puzzles by reducing the average domain size to one, a surprising feat given that AC3 is not meant to generate solutions. However, this was not the case for
any of the harder puzzles. \\ 


\begin{table}[h]
    \centering
    \caption{\textit{The Number of Board Combinations Before and After AC3.}}
    \begin{tabular}{lcccc}
        \toprule
        Puzzle Difficulty & \multicolumn{2}{c}{Easy} & \multicolumn{2}{c}{Hard} \\
        \cmidrule(lr){2-5}
        & M & SD & M & SD \\
        \midrule
        Before AC3 & 49.64 & 3.50 & 57.44 & 3.11 \\
        After AC3 & 19.38 & 11.80 & 36.95 & 4.25 \\
        \bottomrule
        \multicolumn{5}{p{8cm}}{\textit{Note.} Values are given in log base 10. M stands for mean and SD for standard deviation.}
    \end{tabular}
    \label{tab:ac3_comparison}
\end{table}

\begin{figure}[h]
    \centering
    \caption{\textit{Average Domain Size Before and After AC3.}}
    \includegraphics[width=0.8\textwidth]{benchmark/Average domain size boxplot.pdf}
    \label{fig:domainSizeReduction}
\end{figure}

\hide{

With these function we can define the main loop that the user interacts with. It asks the user to choose a sudoku puzzle, and then runs AC3 and backtracking on it.
The user can choose between easy, hard, and special puzzles. Easy and hard puzzles are chosen by number, while special puzzles are chosen by name. Each of these
three cases are considered, and the user is prompted to choose again if an invalid choice is made.


\begin{code}

-- | The main interface interacting with the user
sudokuMain :: IO ()
sudokuMain = do
    showWelcomeMessage

    putStr "Choose your difficulty: \n\
         \  (1) easy\n\
         \  (2) hard\n\
         \  (3) special\n\n\
         \Select one of 1/2/3: "
    hFlush stdout -- ^ Flush the output buffer to ensure prompt is displayed immediately

    diff <- getLine

    fileName <- case diff of

        -- | Easy puzzle case
        "1" -> do getEasyPuzzle where -- ^ Start the recursive prompt
            getEasyPuzzle = do
                putStr "Choose a puzzle number between 1 and 50: "
                hFlush stdout

                puzzleNum <- getLine
                -- ^ Check if input is a valid number in range
                case reads puzzleNum :: [(Int, String)] of
                    [(num, "")] | num >= 1 && num <= 50 -> 
                        return ("easy" ++ puzzleNum)
                    _ -> do
                        putStrLn $ "Sorry, " ++ show puzzleNum ++ " is an invalid choice. Please enter a number between 1 and 50."
                        getEasyPuzzle -- ^ Try again

        -- | Hard puzzle case
        "2" -> do getHardPuzzle where -- ^ Start the recursive prompt
            getHardPuzzle = do
                    putStr "Choose a puzzle number between 1 and 95: "
                    hFlush stdout

                    puzzleNum <- getLine
                    -- ^ Check if input is a valid number in range
                    case reads puzzleNum :: [(Int, String)] of
                        [(num, "")] | num >= 1 && num <= 95 -> 
                            return ("hard" ++ puzzleNum)
                        _ -> do
                            putStrLn $ "Sorry, " ++ show puzzleNum ++ " is an invalid choice. Please enter a number between 1 and 95."
                            getHardPuzzle -- ^ Try again
        
        -- | Special puzzle case
        "3" -> do askForSpecialPuzzle where -- ^ Start the recursive prompt
            askForSpecialPuzzle = do
                putStr "Choose a puzzle: \n\
                    \  (1) Impossible\n\
                    \  (2) Mirror\n\
                    \  (3) Times1\n\n\
                    \Select one of (1, 2, 3): "
                hFlush stdout
                
                puzzleName <- getLine

                case puzzleName of
                    "1" -> return "impossible"
                    "2" -> return "Mirror"
                    "3" -> return "Times1"
                    _   -> do
                        putStrLn $ "Sorry, " ++ show puzzleName ++ " is not a valid choice. Please try again."
                        askForSpecialPuzzle -- ^ Try again
        
        -- | Invalid choice
        x -> do
            putStrLn $ "Sorry, " ++ show x ++ " is not a valid choice. Please try again."
            sudokuMain  -- ^ Restart if invalid choice
            return ""   -- ^ This line is dealt with below

    if null fileName then return () -- ^ fileName is null after user executes case x, but the program has already successfully run
    else do
        -- ^ Solve the Sudoku puzzle from the file
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
}









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

domainCombinationsExperiment :: String -> IO (String, String)
domainCombinationsExperiment fileName = do
    (_, _, oldDomains, newDomains) <- runAC3OnSudokuFileSilent fileName
    
    let oldCombinations = numberofCombinations oldDomains
    let newCombinations = numberofCombinations newDomains

    return (oldCombinations, newCombinations)

    where
        numberofCombinations :: [[Int]] -> String
        numberofCombinations domains' = "10^" ++ show flooredValue where
            a = map length domains'
            logProduct = sum $ map (logBase 10 . fromIntegral) a :: Float
            flooredValue = floor logProduct :: Int

runDomainCombinationsExperiment :: IO ()
runDomainCombinationsExperiment = do
    results <- mapM domainCombinationsExperiment hardFiles
    print results

\end{code}


}
