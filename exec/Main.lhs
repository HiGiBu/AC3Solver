
\section{Wrapping it up in an exectuable}\label{sec:Main}

% We will now use the library form Section \ref{sec:Basics} in a program.
TODO

\begin{code}
module Main where

import Text.Read (readMaybe)

import GraphCol
import Knapsack
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
  putStrLn "Hello!"
  --print somenumbers
  --print (map funnyfunction somenumbers)
  --myrandomnumbers <- randomnumbers
  --print myrandomnumbers
  --print (map funnyfunction myrandomnumbers)
  --putStrLn "GoodBye"

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

We can run this program with the commands:

\begin{verbatim}
stack build
stack exec myprogram
\end{verbatim}

