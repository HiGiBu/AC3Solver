\section{Graph Colouring tests}
\begin{code}
module Main where 

import AC3Solver
import Backtracking
import GraphCol

import Data.Graph
import Data.List (sort)
import Data.Maybe
import Test.Hspec
import Test.QuickCheck

replaceDomainN :: Int -> GraphCol -> GraphCol 
replaceDomainN n (GC (AC3 c d)) = GC (AC3 c ((0,[0]):[ (x,[0..n]) | (x,_)<- tail d]) )

main :: IO ()
main = hspec $ do
  describe "Graph colouring Tests" $ do
    it "Vertex 0 should always have as domain only the colour 0" $ 
      property (\(GC inst) -> (0, [0]) `elem` domains inst)  
    
    it "Each constraint has an edge in the graph of the instance" $ 
      property (\(GC inst) -> let g=ac3ToGraph (GC inst) in all (`elem` edges g) [ (x,y) | (x,y,_)<- cons inst ]) 
    it "Each edge in the graph has a matching constraint in the instance" $ 
      -- note: as we don't have Eq defined for a constraint (and can't, as it's a function), 
      --        we need to extract the pairs from the constraint. 
      property (\(GC inst) -> let g=ac3ToGraph (GC inst) in all (\(x,y) -> (x,y) `elem` [ (a,b) | (a,b,_)<-cons inst]) $ edges g) 

    it "Running AC3 a second time does not change the actual output, only possibly the order." $ 
      property (\(GC inst) -> let 
        newD = ac3 inst
        newD2 = ac3 $ AC3 (cons inst) newD
        in sort newD == sort newD2)

    it "Converting a graph to GraphCol and back should not lose edges" $ 
      property (\gc -> let -- not ideal to have to use gc...
          g = ac3ToGraph gc
          newGC = convertGraphToAC3 g 1 
          newG = ac3ToGraph newGC
          in all (`elem` edges newG) $ edges g)
      
    it "Any found solution should also be in the list of all solutions" $ 
      property (\(GC inst) -> let 
          msol = findSolution inst
          in case msol of 
            Nothing -> True 
            Just sol -> sol `elem` findAllSolutions inst
          )

    it "If an agent has an empty domain, then no solution can be found" $ 
      property (\(GC inst) -> let 
          isSol = determineNoSol $ ac3 inst
          -- if there is defintely no solution, then findSol should indeed return Nothing.
          in not isSol || isNothing (findSolution inst)
          )


    -- yeah so this is just Wrong... but maybe some of the code is useful in the future 
    {-
    it "G 2-colourable iff even degree" $ do 
        -- a graph G is 2-colourable iff it is bipartite  
      property (\(GC inst) -> let
         (GC newInst) = replaceDomainN 2 (GC inst)
         newD = ac3 newInst
         hasSol = isJust $ findSolution (AC3 (cons inst) newD)
         g = ac3ToGraph (GC newInst)
         outD = outdegree g 
         --tem = foldr (\d boolV -> even d && boolV ) True outD
         evenD = foldr (\d boolV -> even d && boolV ) True outD
         in evenD == hasSol)
      -}
      -- TODO: newDomain does not change underlying graph
\end{code}