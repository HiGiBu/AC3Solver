\section{Graph Colouring tests}
\begin{code}
module Main where 

import AC3Solver
import Backtracking
import GraphCol

import Data.Maybe
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "AC3 Tests" $ do
    it "Vertex 0 should always have as domain only the colour 0" $ 
      property (\(GC g) -> (0, [0]) `elem` domains g)  


\end{code}