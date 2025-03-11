
Giga mess, lots of TODOs and optimisations etc., but this seems to work for the simple
    AC3 test cases I have below:

\begin{code}
module AC3_Mess where

-- TODO: Use sequence to implement a priority queue-type thing for the constraints?
--import Data.Sequence (Seq, ViewR(..), ViewL(..), (<|), (|>), (><))
--import qualified Data.Sequence as Seq -- https://hackage.haskell.org/package/containers-0.8/docs/Data-Sequence.html

import Control.Monad.Writer
    ( runWriter, MonadWriter(tell), Writer )

import Data.List
    ( delete )

data AC3 a b = AC3 { 
        -- Constraint should take values from the first & second agents as params x & y resp. in \x,y-> x ?=? y. 
        -- We should allow for multiple constraints for (X,Y), eg. both (x > y) AND (x < y) in the set. 
        cons :: [ConstraintAA a b], 
        -- Assume we have 1 domain list for each variable. (TODO: Check for this? )
        domains :: [Domain a b] } 


    --deriving (Eq,Ord,Show) -- requires this from a & b...
    -- Plus, I doubt we can use this given constraint is a function...

type Domain a b = (Agent a, [b])
type ConstraintAA a b = (Agent a, Agent a, Constraint b)
type Constraint a = a -> a -> Bool

type Agent a = a 

-- Return the elements of xs for which there exist a y \in ys, such that c x y holds.
checkDomain :: [a] -> [a] -> Constraint a -> [a]
checkDomain xs ys c = filter (`checkX` ys) xs where 
    checkX _ [] = False
    checkX x' (y:ys') = c x' y || checkX x' ys'

-- Return the elements of xs for which there exist a y \in ys, such that c x y holds.
-- Using the writer monad, we also give a O(1) method to check whether we altered x's domain after termination.
checkDomain2 :: [a] -> [a] -> Constraint a -> Writer String [a]
checkDomain2 [] _ _ = return []
checkDomain2 (x:xs) ys c = do 
    rest <- checkDomain2 xs ys c  
    if not $ null [ y' | y'<-ys, c x y'] then return $ x:rest
    else tell "Altered domain" >> return rest -- This is nicely formatted for readability, it could just be "." or whatever.

-- Then we can use with this with: runWriter $ checkDomain2 xs ys c
-- newX = fst result, output = snd result. 
-- If output == "", then domain unchanged, and we can simply forget the constraint.
-- Else, look through original set of constraints, and re-add all constraints for ys.


ac3 :: (Ord a, Ord b) => AC3 a b -> [Domain a b] -- return a list of domains.
ac3 m@(AC3 c d) = let 
    queue = c -- put each constraint into the queue.  -- TODO: implement this better, eg a priority queue?
    in iterateAC3 m queue d

iterateAC3 :: (Ord a, Ord b) => AC3 a b -> [ConstraintAA a b] -> [Domain a b] -> [Domain a b]
iterateAC3 _ [] d = d
iterateAC3 m@(AC3 fullCS _) ((x,y,c):cs) d = let 
    xDomain = head [b | (a,b)<-d, x==a ]
    yDomain = head [b | (a,b)<-d, y==a ]
    (newX, str) = runWriter $ checkDomain2 xDomain yDomain c
    -- In a lens, we could do this with "modify (\ (a,_) -> (a, newX))"
    newDomains = (x, newX) : ((x, xDomain) `delete` d) 
    z = if null str then cs else cs ++ [c' | c'@(y1,x1,_)<-fullCS, y1==y, x1==x ] -- take all constraints of the form (y,x, c) 
    in iterateAC3 m z newDomains 

\end{code}

And some test cases, for the graph colouring problem. If I'm not mistaken, 
G is n-colourable (for given n, assuming correct definition of instance) 
iff AC3 outputs at least 1 option in the domain of each Agent.

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

\end{code}