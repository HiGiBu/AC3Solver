\subsection{The NQueens library}\label{sec:NQueens}

The NQueens module defines a \text{constraint satisfaction problem} where we place $N$ queens on an $N \times N$ chessboard so that no two queens attack each other.

\begin{code}
module NQueens where

import AC3Solver ( ac3, AC3(AC3) )  -- Import AC3 solver
import Backtracking (findSolution, findAllSolutions ) -- Import backtracking solver

notSameQueenMove :: (Int, Int) -> (Int, Int) -> Bool
notSameQueenMove (a1, a2) (b1, b2) =
    not (a1 == b1 || a2 == b2 || abs (a1 - b1) == abs (a2 - b2))

(//=) :: (Int, Int) -> (Int, Int) -> Bool
(a1, a2) //= (b1, b2) = notSameQueenMove (a1, a2) (b1, b2)

\end{code}

The \texttt{nQueens} function encodes the N-Queens problem as a constraint satisfaction problem. 
The domain is defined in such a way that exactly one queen must be placed in each row. 
Constraints are generated using list comprehension together with the custom infix function \texttt{(//=)}, which ensures that no two queens share the same row, column, or diagonal.

\begin{code}
nQueens :: Int -> AC3 Int (Int, Int)
nQueens n = let
    agents = [0 .. n-1] -- Queens as row numbers
    domain = [(row, [(row, col) | col <- [0 .. n-1]]) | row <- agents] -- 1 queen per row
    constraints = [(a, b, (//=)) | a <- agents, b <- agents, a < b]
    in AC3 constraints domain
\end{code}

There are two functions available to solve the problem. 
The function \texttt{solveNQueens} finds a single solution using backtracking. 
Meanwhile, the function \texttt{solveAllNQueens} finds all possible solutions.

\begin{code}
solveNQueens :: Int -> Maybe [(Int, (Int, Int))]
solveNQueens n = findSolution (AC3 constraints (ac3 (nQueens n)))
  where
    AC3 constraints _ = nQueens n

solveAllNQueens :: Int -> [[(Int, (Int, Int))]]
solveAllNQueens n = findAllSolutions (AC3 constraints (ac3 (nQueens n)))
  where
    AC3 constraints _ = nQueens n
\end{code}

The function \texttt{prettyPrintBoard} is responsible for printing the board. 
Solutions are displayed using numbers (\texttt{0,1,2,...}) to represent queens, while empty spaces are represented by a dot (\texttt{.}).

\begin{code}
prettyPrintBoard :: Int -> [(Int, (Int, Int))] -> IO ()
prettyPrintBoard n solution = do
    let board = [[if (r, c) `elem` map snd solution then show r else "." | c <- [0 .. n-1]] | r <- [0 .. n-1]]
    mapM_ (putStrLn . pptHelper) board
    putStrLn ""

pptHelper :: [String] -> String
pptHelper [] = ""
pptHelper [x] = x
pptHelper (x:xs) = x ++ " " ++ pptHelper xs
\end{code}

The \texttt{nQueensMain} function provides user interaction by asking for an input value $N$.
It then solves the problem and either prints the full solutions or just the count of solutions, depending on whether \texttt{prettyPrintBoard} is enabled. 
To start the NQueens program, run \texttt{stack ghci} and then \texttt{nQueensMain}, after which you are prompted to give an integer for $N$.

\begin{code}
nQueensMain :: IO ()
nQueensMain = do
    putStrLn "Enter board size (N):"
    n <- readLn
    let solutions = solveAllNQueens n
    -- Uncomment for 1 solution instead
    -- let solutions = solveNQueens n
    if null solutions
    then putStrLn "No solution found."
    else do
        putStrLn "Solutions: "
        -- Comment out if only interested in the number of solutions
        -- mapM_ (prettyPrintBoard n) solutions
        putStrLn $ "Found " ++ show (length solutions) ++ " solution(s)"
\end{code}
