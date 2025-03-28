
\section{Running the executable}\label{sec:Main}

% We will now use the library form Section \ref{sec:Basics} in a program.
% TODO
By running \verb:stack exec myprogram:, we can run the main programme. 
Here, we can choose which problem instance to work on, at which point the user is 
sent to the main function for the chosen problem.

\begin{code}
module Main where

import Text.Read (readMaybe)

import GraphCol
import Scheduling
import Sudoku
import NQueens
import ZebraPuzzle

getChoice :: IO Int
getChoice = do 
  putStr "Choose one of the following options: \n\
         \1: Graph Colouring \n\
         \2: N-Queens \n\
         \3: Scheduling \n\
         \4: Sudoku \n\
         \5: Zebra Puzzle \n"
  choice <- getLine 
  case readMaybe choice of 
    Nothing -> do
      putStrLn "Invalid choice, please try again."
      getChoice
    Just n -> 
      if n > 0 && n < 6 then return n else do 
        putStrLn "Invalid choice, please try again."
        getChoice

main :: IO ()
main = do
  putStrLn "Welcome to our AC-3 solver."

  -- Get choice
  choice <- getChoice
  case choice of 
    1 -> graphColMain
    2 -> nQueensMain
    3 -> schedulingMain
    4 -> sudokuMain
    5 -> zebraPuzzleMain
    _ -> undefined

\end{code}

% We can run this program with the commands:

% \begin{verbatim}
% stack build
% stack exec myprogram
% \end{verbatim}

