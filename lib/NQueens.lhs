\subsection{The NQueens library}\label{sec:NQueens}

\begin{code}
module NQueens where

import AC3Solver ( ac3, AC3(AC3) )  -- import AC3 solver
import Backtracking (findSolution, findAllSolutions ) -- import backtracking solver

notSameQueenMove :: (Int, Int) -> (Int, Int) -> Bool
notSameQueenMove (a1, a2) (b1, b2) =
    not (a1 == b1 || a2 == b2 || abs (a1 - b1) == abs (a2 - b2))

(//=) :: (Int, Int) -> (Int, Int) -> Bool
(a1, a2) //= (b1, b2) = notSameQueenMove (a1, a2) (b1, b2)

nQueens :: Int -> AC3 Int (Int, Int)
nQueens n = let
    agents = [0 .. n-1] -- queens as row numbers
    domain = [(row, [(row, col) | col <- [0 .. n-1]]) | row <- agents] --1 queen per row
    constraints = [(a, b, (//=)) | a <- agents, b <- agents, a < b]
    in AC3 constraints domain

-- uses old backtracking -- a single solution (unused) 
solveNQueens :: Int -> Maybe [(Int, (Int, Int))]
solveNQueens n = findSolution (AC3 constraints (ac3 (nQueens n)))
  where
    AC3 constraints _ = nQueens n

-- find all solutions
solveAllNQueens :: Int -> [[(Int, (Int, Int))]]
solveAllNQueens n = findAllSolutions (AC3 constraints (ac3 (nQueens n)))
  where
    AC3 constraints _ = nQueens n

-- simple pretty print using number as queens and . as empty spaces
prettyPrintBoard :: Int -> [(Int, (Int, Int))] -> IO ()
prettyPrintBoard n solution = do
    let board = [[if (r, c) `elem` map snd solution then show r else "." | c <- [0 .. n-1]] | r <- [0 .. n-1]]
    mapM_ (putStrLn . pptHelper) board
    putStrLn ""

pptHelper :: [String] -> String
pptHelper [] = ""
pptHelper [x] = x
pptHelper (x:xs) = x ++ " " ++ pptHelper xs

nQueensMain :: IO ()
nQueensMain = do
    putStrLn "Enter board size (N):"
    n <- readLn
    let solutions = solveAllNQueens n
    -- uncomment for 1 solution instead
    -- let solutions = solveNQueens n
    if null solutions
    then putStrLn "No solution found."
    else do
        putStrLn "Solutions: "
        -- comment out if only interested in the number of solutions
        -- mapM_ (prettyPrintBoard n) solutions
        putStrLn $ "Found " ++ show (length solutions) ++ " solution(s)"
\end{code}