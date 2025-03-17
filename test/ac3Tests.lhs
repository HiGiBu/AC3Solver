\section{AC3 tests}
\begin{code}
module Main where 

import AC3Solver

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "AC3 Tests" $ do
    it "Example test" $ 
      ac3 exampleAC3 `shouldBe` [(4,[1,2]),(3,[0,1,2]),(2,[0,1,2]),(1,[0,1,2]),(0,[0])]

\end{code}

\begin{code}
-- TEST CASES

exampleAC3 :: AC3 Int Int 
exampleAC3 = let 
    nColours = 3 
    nAgents = 5
    -- we assign a specific starting value to an (arbitrary) node. (TODO: for general encoding, if a vertex has no edges, assign an arbit colour.)
    in AC3 [ (a, (a+1) `mod` nAgents, (/=)) | a<-[0..nAgents-1]] ((0, [0]) : [ (a, [0..nColours-1]) | a<-[1..nAgents-1]])


-- A graph is 2-colourable iff it is bipartite iff it has no cycles of odd length.
-- (Such as, this example which is a circle of even length.)
exampleAC3_2 :: AC3 Int Int 
exampleAC3_2 = let 
    nColours = 2 
    nAgents = 6
    -- we assign a specific starting value to an (arbitrary) node.
    in AC3 ([ (a, (a-1) `mod` nAgents, (/=)) | a<-[0..nAgents-1]]++[ (a, (a+1) `mod` nAgents, (/=)) | a<-[0..nAgents-1]]) 
           ((0, [0]) : [ (a, [0..nColours-1]) | a<-[1..nAgents-1]])

-- NOT 2-colourable, as it has an odd cycle (circle of len 5). 
exampleAC3_bad :: AC3 Int Int 
exampleAC3_bad = let 
    nColours = 2 
    nAgents = 5
    -- we assign a specific starting value to an (arbitrary) node.
    in AC3 ([ (a, (a-1) `mod` nAgents, (/=)) | a<-[0..nAgents-1]]++[ (a, (a+1) `mod` nAgents, (/=)) | a<-[0..nAgents-1]]) 
           ((0, [0]) : [ (a, [0..nColours-1]) | a<-[1..nAgents-1]])

-- NOT 1-colourable, as it has an edge.
exampleAC3_triv :: AC3 Int Int 
exampleAC3_triv = let 
    nColours = 1 -- can only be 1-colourable iff cons = [].
    nAgents = 5
    -- we assign a specific starting value to an (arbitrary) node.
    in AC3 ([ (a, (a+1) `mod` nAgents, (/=)) | a<-[0..nAgents-1]]) 
           ((0, [0]) : [ (a, [0..nColours-1]) | a<-[1..nAgents-1]])

-- Example based on https://www.geeksforgeeks.org/3-coloring-is-np-complete/
-- IS 3-colourable.
exampleAC3_GFG :: AC3 String Int 
exampleAC3_GFG = let 
    nColours = 3 -- can only be 1-colourable iff cons = []. 
    agentsA = ["v", "w", "u", "x"]
    agentsB = [s++"'" | s<-agentsA]
    agents = agentsA ++ agentsB -- does NOT include "B"

    bCons = [("B", a, (/=)) | a<-agents]
    outsideCons = [ (a, a++"'", (/=)) | a<-agentsA ]
    reverseCons = map (\ (a,b,c) -> (b,a,c))
    in AC3 (bCons ++ reverseCons bCons ++ 
                outsideCons ++ reverseCons outsideCons) 
           ( ("B", [0]) : [ (a, [0..nColours-1]) | a <- agents])

-- A problem that should have no solutions
exampleAC3_no_solution :: AC3 Int Int 
exampleAC3_no_solution = let
    domains_no_sol = [(0, [1,2]), (1, [1,2]), (2, [1,2])]
    constraints_no_sol = [(0, 1, (/=)), (1, 2, (/=)), (0,2, (/=))]
    in AC3 constraints_no_sol domains_no_sol

\end{code}