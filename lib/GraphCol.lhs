\begin{code}
--{-# LANGUAGE LambdaCase #-} -- todo remove? if not using data.graph.read...
module GraphCol where

import Control.Monad (when)
import Data.Char (toUpper)
import Data.Graph
--import Data.Graph.Read
import Data.List
import Text.Read (readMaybe)
import Test.QuickCheck

import AC3Solver
import Backtracking
import Scheduling (parseInput)

-- We define a newtype, so that we can generate arbitrary instances.
newtype GraphCol = GC (AC3 Vertex Int)

seqPair :: (Gen a, Gen a) -> Gen (a,a)
seqPair (ma, mb) = ma >>= \a -> mb >>= \b -> return (a,b) 

-- (Seems graphs don't already have an arbitrary instance...)
instance Arbitrary GraphCol where 
    arbitrary = sized arbitGraphColN where 
        arbitGraphColN n = do 
            nColours <- choose (0, n `div` 2)
            sizeV <- choose (0, n) -- we make vertices 0..sizeV INCLUDING SIZEV!!
            let v = [0..sizeV] -- create vertices 0..x INCLUDING SIZEV
            sizeE <- choose (0,n)
            e <- sequence [seqPair (choose (0, sizeV), choose (0, sizeV)) | _<-[0..sizeE]]
            let g = buildG (0, sizeV) e
            return $ convertGraphToAC3 g nColours --return $ convertGraphToAC3 g n

instance Show GraphCol where 
  --show :: GraphCol -> String 
  show (GC (AC3 c d)) = let 
    strCon = "[" ++ makeShow c ++ "]" where 
      makeShow [] = ""
      makeShow ((x,y,_):cs) = 
        "(" ++ show x ++ ", " ++ show y ++ ", (/=))" 
        ++ if not $ null cs then ", " ++ makeShow cs else "" 
    strD = show d
    in "GC (AC3 " ++ strCon ++ " " ++ strD ++ " )" 

\end{code}

We define a method to convert a graph into an instance of \verb:GraphCol:, and vice versa.
Note that graph colouring concerns \emph{un}directed graphs while the Graph library 
concerns \emph{directed} graphs. As a result, \verb:g == ac3ToGraph $ convertGraphToAC3 g n: (for any $n>0$) is NOT
guaranteed to hold.

\begin{code}

-- NOTE: The Graph library uses *directed* graphs. 
--        We add both (x,y,/=) and (y,x,/=), as graph colouring concerns Undirected graphs.
-- Create an instance with colours [0..(n-1)]
convertGraphToAC3 :: Graph -> Int -> GraphCol 
convertGraphToAC3 g n = let     
    agents = vertices g
    constr = [(x,y, (/=)) | (x,y)<-edges g]
    in GC $ AC3 
        (constr ++ reverseCons constr) 
        -- In graph colouring, we want to check both X's domain to Y, and Y's to X.
        ((head agents, [0]) : [(a, [0..(n-1)]) | a<-tail agents]) 

-- Help function: If we have an edge (x,y), we need both (x,y, /=) and (y,x,/=) as constraints.
reverseCons :: [(a,b,c)] -> [(b,a,c)]
reverseCons = map (\ (a,b,c) -> (b,a,c))

ac3ToGraph :: GraphCol -> Graph 
ac3ToGraph (GC (AC3 c d)) = let 
    v = [a | (a,_)<-d]
    e = nub [ (a,b) | (a,b,_)<-c] -- If we originally had (x,y) AND (y,x) in our graph, then c contains each twice.
    in buildG (foldr min 0 v, foldr max minBound v) e



{-
-- TODO Remove?
ac3GetNcolours :: GraphCol -> Int 
ac3GetNcolours (GC (AC3 _ (d':ds))) = (\(_,xs) -> foldr max 0 xs) d'
ac3GetNcolours (GC (AC3 _ d)) = foldr (\(_,xs) -> foldr max 0 xs) 0 d
-}

\end{code}

The actual main part of the programme, for Graph Colouring:

\begin{code}

getGraphChoice :: IO Int
getGraphChoice = do 
  putStr "Choose one of the following options: \n\
         \1: Read in a graph from the terminal \n\
         \2: TODO \n"
  choice <- getLine 
  case readMaybe choice of 
    Nothing -> do
      putStrLn "Invalid choice, please try again."
      getGraphChoice
    Just n -> 
      if n > 0 && n < 2 then return n else do 
        putStrLn "Invalid choice, please try again."
        getGraphChoice

graphColMain :: IO ()
graphColMain = do 
    choice <- getGraphChoice
    case choice of 
        1 -> terminalGraph
        _ -> undefined

-- PRE: m <= n.
getEdges :: Int -> Int -> IO [Edge]
getEdges m n 
  | m == n = return [] 
  | otherwise = do 
      putStrLn $ "Edge " ++ show m
      x <- parseInput "Enter the first vertex: "
      y <- parseInput "Enter the second vertex: "

      rest <- getEdges (m+1) n 
      return $ (x,y) : rest

terminalGraph :: IO () 
terminalGraph = do 
  nVertices <- parseInput "Enter the number of vertices: "
  putStrLn $ "Okay, we number the vertices from 0 to " ++ show (nVertices-1) 
  -- TODO: Error handling, eg. if nVertices <= 0 ?
  nEdges <- parseInput "Enter the number of edges: "
  eList <- getEdges 0 nEdges 
  let graph = buildG (0, nVertices-1) eList
  nColours <- parseInput "Enter the number of colours: "
  let g = convertGraphToAC3 graph nColours 
  putStrLn "We run AC3 on this instance."
  runGraph g


-- Given a graphcol instance, we run AC3 on it. If we have at least 1 solution (after back prop.),
--  show it to the user,  and ask if they want to see all solutions.
runGraph :: GraphCol -> IO ()
runGraph (GC ac3Inst) = do 
  let ac3Domain = ac3 ac3Inst 
  if determineNoSol ac3Domain 
    then putStrLn "AC3 has found an empty domain for at least 1 agent -> No solution"
    else do 
      putStrLn "AC3 has at least 1 option for each agent."
      case findSolution ac3Inst of 
        Nothing -> putStrLn "There is no solution based on the reduced AC3 input."
        Just sol -> do 
          putStrLn $ "We have found a solution: " ++ show sol 
          putStrLn "Do you want to find out how many different solutions we have? (Y/N) "
          choice <- getLine 
          when (toUpper (head choice) == 'Y') $ do 
            let allSols = findAllSolutions ac3Inst
            if length allSols == 1 then putStrLn "There is only 1 solution." 
            else do -- it should not be possible to reach here if allSols = 0.
              putStrLn $ "There are " ++ show (length allSols) ++ " different solutions. \nDo you want to see them? (Y/N) "
              choice2 <- getLine
              when (toUpper (head choice2) == 'Y') $ mapM_ print allSols
            





-- Note: The graph we read in must be a CSV of an adjecency list; 
-- https://hackage.haskell.org/package/graphite-0.10.0.1/docs/Data-Graph-Read.html
csvGraph :: IO ()
csvGraph = undefined
{-csvGraph = do 
    putStrLn "Give a filepath to the graph you want to read in."
    filePath <- getLine 
    x <- fromCsv filePath

    undefined
    --case fromCsv filePath of
    --    Left _ -> undefined
    --    Right _ -> undefined

-}

\end{code}