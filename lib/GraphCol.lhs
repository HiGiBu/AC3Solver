\subsection{The Graph Colouring library}\label{sec:GraphCol}

Graph colouring is a well-known NP-Complete problem~\cite{GraphColRef}. 
Its nature as a graph problem lends it well to being modelled as an \verb:AC3: instance,
 and then being solved using our backtracking functions.

A problem instance consists of an undirected graph, and an integer $n > 0$. 
We are asked to assign a colour $0..(n-1)$ to each vertex, where for each edge $(u,v)$, 
$u$ and $v$ have different colours. 

\begin{code}
module GraphCol where

import Control.Monad (when, foldM_)
import Criterion.Main
import Data.Char (toUpper)
import Data.Graph
import Data.Maybe
import Data.List
import Text.Read (readMaybe)
import Test.QuickCheck

import AC3Solver
import Backtracking
import Scheduling (parseInput)

\end{code}

We make use of Haskell's Graph library, following in its convention that vertices are 
numbers, and edges are pairs of vertices. 

We define a newtype \verb:GraphCol: using \verb:AC3:, where the variables are of type \verb:Vertex: 
and the domain is a set of colours $\subseteq$ \verb:[0..(n-1)]:.
All constraints should be of the form \verb:(X,Y,(/=)):, and this represents an edge \verb:(X,Y):
in the graph. 

We define arbitrary instances for \verb:GraphCol: using following these conventions.

\begin{code}

-- We define a newtype, so that we can generate arbitrary instances.
newtype GraphCol = GC (AC3 Vertex Int)

seqPair :: (Gen a, Gen a) -> Gen (a,a)
seqPair (ma, mb) = ma >>= \a -> mb >>= \b -> return (a,b) 

-- It appears that Haskell graphs do not already have an arbitrary instance.
instance Arbitrary GraphCol where 
    arbitrary = sized arbitGraphColN where 
        arbitGraphColN n = do 
            nColours <- chooseInt (1, max (n `div` 4) 1) -- we require n to be > 0 
            --nColours <- chooseInt (2, max (n `div` 4) 2)
            sizeV <- choose (0, n `div` 3) -- we make vertices 0..sizeV INCLUDING SIZEV!
            --let sizeV = 498
            let eMax = max sizeV $ (sizeV*(sizeV-1)) `div` 4
            sizeE <- chooseInt (sizeV, eMax)
            e <- sequence [seqPair (chooseInt (0, sizeV), chooseInt (0, sizeV)) | _<-[0..sizeE]]
            -- we do not want edges (x,x), nor do we want repeat edges.
            let nonReflE = nub $ filter (uncurry (/=)) e
            let g = buildG (0, sizeV) nonReflE
            return $ convertGraphToAC3 g nColours --return $ convertGraphToAC3 g n

-- | We require show to use the arbritary instance above in QuickCheck.
-- | Note that we cannot actually check that each constraint is a (/=), we must 
-- | assume it to be so.
instance Show GraphCol where 
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

-- | NOTE: The Graph library uses *directed* graphs. 
-- |       We add both (x,y,/=) and (y,x,/=), as graph colouring concerns Undirected graphs.
-- | Create an instance with colours [0..(n-1)]
-- | PRE: n >= 1
convertGraphToAC3 :: Graph -> Int -> GraphCol 
convertGraphToAC3 g n = let     
    variables = vertices g
    constr = [(x,y, (/=)) | (x,y)<-edges g]
    in GC $ AC3 
        (constr ++ reverseCons constr) 
        -- In graph colouring, we want to check both X's domain to Y, and Y's to X.
        ((head variables, [0]) : [(a, [0..(n-1)]) | a<-tail variables]) 

-- | Help function: If we have an edge (x,y), we need both (x,y, /=) and (y,x,/=) as constraints.
reverseCons :: [(a,b,c)] -> [(b,a,c)]
reverseCons = map (\ (a,b,c) -> (b,a,c))

-- | Given a graph colouring instance, return the graph of it. 
-- | POST: Edges contains at most 1 Directed edge (x,y) for all vertices x/=y.
ac3ToGraph :: GraphCol -> Graph 
ac3ToGraph (GC (AC3 c d)) = let 
    v = [a | (a,_)<-d]
    e = nub [ (a,b) | (a,b,_)<-c] -- If we originally had (x,y) AND (y,x) in our graph, then c contains each twice.
    in buildG (foldr min 0 v, foldr max minBound v) e



\end{code}

We provide a section of code that may optimise the \verb:GraphCol: instance.
We assign the colour 0 to the vertex 0, as in graph colouring we can arbitrarily
assign a colour to the `first' vertex. 

However, if the graph consists of multiple disconnected components, then we can 
do such an arbitrary assignment to a vertex in each separate component, thereby reducing 
the search space. 

\begin{code}

optimiseGC :: GraphCol -> GraphCol
optimiseGC gc@(GC (AC3 c d)) = let  
  comps = components $ ac3ToGraph gc
  -- As far as I can find with the tests, if 0 is an element of a component, then
  --  0 is at the root. (Assuming a normal, legal GC instance of course). 
  -- We assume this is the case. For each component, we assign the reduced domain [0]
  -- to the root, thereby reducing the search space.
  dChanges = map (\(Node r _) -> r) comps
  in GC (AC3 c (map (\(a,b) -> if a `elem` dChanges then (a,[0]) else (a,b)) d))


\end{code}

\hide{ %We leave out this part from the report, due to space considerations.
The actual main part of the programme, for Graph Colouring:

\begin{code}

graphColMain :: IO ()
graphColMain = do 
    choice <- getGraphChoice
    case choice of 
        1 -> terminalGraph
        2 -> fileGraph
        3 -> benchmarkTests
        _ -> undefined

getGraphChoice :: IO Int
getGraphChoice = do 
  putStr "Choose one of the following options: \n\
         \1: Read in a graph colouring instance from the terminal \n\
         \2: Read in a graph colouring instance from a file \n\
         \3: Run benchmarks \n"
  choice <- getLine 
  case readMaybe choice of 
    Nothing -> do
      putStrLn "Invalid choice, please try again."
      getGraphChoice
    Just n -> 
      if n > 0 && n < 4 then return n else do 
        putStrLn "Invalid choice, please try again."
        getGraphChoice

-- | While m < n, read in an edge (= 2 vertices = 2 integers). 
-- | PRE: m <= n.
getEdges :: Int -> Int -> IO [Edge]
getEdges m n 
  | m == n = return [] 
  | otherwise = do 
      putStrLn $ "Edge " ++ show m
      x <- parseInput "Enter the first vertex: "
      y <- parseInput "Enter the second vertex: "

      rest <- getEdges (m+1) n 
      return $ (x,y) : rest

-- | Reads in a graph from the terminal, then calls runGraph.
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
  putStrLn $ "We run AC3 on this instance: " ++ show g
  runGraph g


-- | Given a graphcol instance, we run AC3 on it. If we have at least 1 solution (after back prop.),
-- |  show it to the user,  and ask if they want to see all solutions.
runGraph :: GraphCol -> IO ()
runGraph (GC ac3Inst) = do 
  let ac3Domain = ac3 ac3Inst 
  if determineNoSol ac3Domain 
    then putStrLn "AC3 has found an empty domain for at least 1 variable -> No solution"
    else do 
      putStrLn "AC3 has at least 1 option for each variable. Checking for solution..."
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
            
\end{code}

} % \hide

\hide{ % same reason as above
\begin{code}

-- | Reads in a graph from a given file.
readGraphFromFile :: String -> IO GraphCol
readGraphFromFile filename = do 
  filecon <- readFile filename 
  let fileInput = words filecon
  let nVertices = read $ head fileInput
  let nEdges = read $ head (tail fileInput)
  let edgeList = makeEdges nEdges (drop 2 fileInput) -- includes nColours
  let nColours = read $ last fileInput
  let graph = buildG (0, nVertices-1) edgeList
  let g = convertGraphToAC3 graph nColours 
  return g

fileGraph :: IO ()
fileGraph = do  
  putStrLn "We expect the file to be of the following format: \n\
            \[number of vertices] \n\
            \[number of edges] \n\
            \for each edge: [vertex 1] [vertex 2] of the edge. \n\
            \[the number of colours > 0.]\n"
  putStrLn "Provide the file name:"
  filename <- getLine 
  {- 
  filecon <- readFile filename 
  let fileInput = words filecon
  let nVertices = read $ head fileInput
  let nEdges = read $ head (tail fileInput)
  let edgeList = makeEdges nEdges (drop 2 fileInput) -- includes nColours
  let nColours = read $ last fileInput
  let graph = buildG (0, nVertices-1) edgeList
  let g = convertGraphToAC3 graph nColours 
  -}
  g <- readGraphFromFile filename
  putStrLn $ "We run AC3 on this instance: " ++ show g
  runGraph g

-- | Given an int & a list of strings, we try to read these in and make edges from them.
-- | PRE: n >= 0.
makeEdges :: Int -> [String] -> [Edge]
makeEdges 0 _ = []
-- if n > 0, but we have run out of edges -> fail
makeEdges _ [] = error "Not enough edges provided"
makeEdges _ [_] = error "Incomplete edge provided (only 1 vertex)."
makeEdges n (x:y:es) = (read x, read y) : makeEdges (n-1) es

-- | Given a GraphCol instance, print it in the format used to read in from files.
graphFileFormat :: GraphCol -> IO () 
graphFileFormat (GC (AC3 c d)) = do 
  --let nVertices = (fst . last . sort) d 
  -- nVertices
  -- succ, as we want vertices from (0..n-1)
  (print . succ . fst . maximum) d 
  --let nEdges = length c
  -- nEdges
  (print . length) c
  -- print each edge
  foldM_ ( \_ (x,y,_) -> putStrLn $ show x ++ " " ++ show y) () c
  -- print nColours
  print . succ $ foldr (\(_,ds) x -> foldr max x ds) 0 d

\end{code}

\begin{code}

-- | A method to note the difference before & after running AC3.
getTotalDomainOptions :: [Domain a b] -> Int 
getTotalDomainOptions = foldr (\(_, ds) prev -> length ds + prev) 0

testFiles :: [String]  
testFiles = map ("graphcolInstances/"++) 
  ["n10e16nc14.txt", "n10e18nc9.txt", "n10e22nc2.txt", "n15e16nc2.txt", 
   "n15e38nc6.txt", "n15e44nc4.txt", 
   "n20e96nc20.txt", "n20e188nc6.txt", 
   "n25e110nc15.txt", "n25e134nc22.txt"]

-- Specific files with loose vertices / 2 separate components
testFilesComps :: [String]
testFilesComps = map ("graphcolInstances/"++) 
  ["n10e26nc7_Single1.txt", "n20e26nc7_Single2.txt", "n20e52nc7_Comps.txt"] 

testFilesComps3 :: [String]
testFilesComps3 = map ("graphcolInstances/"++) 
  ["n10e26nc3_Single1.txt", "n20e26nc3_Single2.txt", "n20e52nc3_Comps.txt"] 

benchmarkTests :: IO ()
benchmarkTests = mapM_ runBenchmark $ testFiles ++ testFilesComps ++ testFilesComps3 ++ ["graphcolInstances/n10e40nc3_Neg.txt"]

-- NOTE: You can also run "stack bench" to run the benchmark for the larger examples.
--        We leave it out here, as compiling these large examples is *very* slow.

\end{code}
} % \hide

\paragraph{Benchmarking GraphCol}~\\
We set out to benchmark \verb:GraphCol: in 2 different ways: given an instance of \verb:GraphCol:, 
first, we determine the total number of options across all domains in the instance, 
  and we compare this to the number of options once we have run \verb:ac3:. We then do the same 
  having run \verb:optimiseGC:, with and without running \verb:ac3:. 

Next, we run \verb:findSolution: on this instance, as well as on the same instance 
altered using \verb:ac3: and/or \verb:optimiseGC:, and record the time using Haskell's 
Criterion library~\cite{criterion}.

\begin{code}
-- | Given a file name, run the benchmark suite on the graphcol instance in this file.
runBenchmark :: String -> IO () 
runBenchmark filename = do
  gc@(GC inst) <- readGraphFromFile filename

\end{code}

Note: Only 1 of the following 2 code blocks should be run, and the other should be hidden.

\begin{code}

  let origNOpts =  getTotalDomainOptions $ domains inst
  let newD = ac3 inst  
  let newNOpts = getTotalDomainOptions newD

  let (GC optiInst) = optimiseGC gc 
  let newOptiD = ac3 optiInst
  let newNOptiD = getTotalDomainOptions newOptiD

  putStrLn $ "Filename: " ++ filename 
  putStrLn $ "Pre AC-3:        " ++ show origNOpts
  putStrLn $ "Post AC-3:       " ++ show newNOpts
  putStrLn $ "OptimiseGC:      " ++ (show . getTotalDomainOptions . domains) optiInst
  putStrLn $ "OptimiseGC AC-3: " ++ show newNOptiD

\end{code}

\begin{code}

  -- Benchmark using Criterion
  defaultMain [
    bgroup filename [ bench "pre AC-3" $ whnf findSolution inst
                    , bench "post AC-3" $ whnf findSolution (AC3 (cons inst) (ac3 inst))
                    , bench "OptimiseGC, no AC-3" $ whnf (\(GC oi) -> findSolution oi) (optimiseGC gc)
                    , bench "OptimiseGC, + AC-3 " $ whnf (\(GC oi) -> findSolution (AC3 (cons inst) (ac3 oi))) (optimiseGC gc)
                    ]
    ]

\end{code}

To get the most fair comparison between the instance before and after running \verb:ac3:, 
we will discusses instances where there is no solution.\footnote{Note that the $ac3$ function can change the order of the domain. As a result, comparing 
            runtimes of $findSolution$ or $findAllSolutions$ may lead to erroneous results.} 
In such cases, \verb:findSolution: 
needs to go through the entire search space, and as such \verb:ac3: is most likely 
to be effective in reducing the run-time. 

We highlight 
one specific example, % two specific cases, 
but all results can be found in the \verb:benchmark: folder. 

% Firstly, the \verb:example_N100_AC3: case, which has 100 vertices, 272 constraints or 270 unique edges, and 2 colours. 
We take the \verb:example_N200_AC3: case, 
which has 200 vertices, 11324 constraints or 10548 unique edges, and 3 colours. 
By running \verb:ac3: on this instance, we can reduce the total number of options 
in the domain from 598 $(200*3 - 2)$ to 553. 
Then, running \verb:findSolution:, the runtime decreases from 119ms on the original instance 
to 45ms running on the reduced instance. 
This is an instance that greatly benefits from running AC-3, as we see the run-time 
reducing as a result of the reduced search space.

% Next, we look at \verb:example_N998_AC3_gc:. % \\: to not make the line overrun, which \verb likes to do
% \verb:N998: is made by effectively duplicating \\ \verb:example_N499_AC3_gc:, 
% where vertex 0 in \verb:N499: is mapped to 0 and 1 in \verb:N998:, 
% vertex 1 in \verb:N499: to 2 and 3 in \verb:N998: etc. 

% Leaving this out, both because the post AC-3 result is a bit weird, and cuz space.
% The point is probably (hopefully?) clear... 

Generally, it is tough to determine whether AC-3 provides much improvement to graph colouring. 
Initially, we only have a limitation on vertex 0's colour being set to 0. 
If we have 1 colour, then AC-3 can solve the instance: if we have 1 or more edges, then 
the variables with not be 1-colourable.  
If we have 2 colours, then AC-3 can assign colours to each vertex in the same component as 0.
If this is not possible, then we can see that the graph is not 2-colourable. 

However for larger N colours, or if the graph contains more than 1 component, 
the only reduction that AC-3 can make is by removing the option for colour 0 for vertices 
connected to vertex 0. Similar to the example mentioned in Section~\ref{sec:intro}, 
AC-3 will not be able to remove options if both vertices have more than 1 colour possible.



\hide{
\begin{code}
-- src: https://hackage.haskell.org/package/safe-0.3.21/docs/Safe.html#v:lookupJust
lookupJust :: (Eq a) => a -> [(a, b)] -> b 
lookupJust key = fromJust . lookup key
\end{code}
}

\hide{ % hidden for space, may be explained in words in the report.
\begin{code}

-- | Given a graph, we want to duplicate it so that we have 2 components, 
-- | where every vertices 1,3,... form 1 copy of the graph, and 0,2,4.. the other
duplicateGraph :: Graph -> Graph 
duplicateGraph g = let 
  mappingEven = zip (vertices g) [0 :: Int, 2..]
  mappingOdd = zip (vertices g) [1 :: Int, 3..]
  newVcount = 2*length (vertices g)  - 1
  newEdges = concatMap (\(a,b) -> [ (lookupJust a mappingEven, lookupJust b mappingEven) ,  (lookupJust a mappingOdd, lookupJust b mappingOdd)]) (edges g)
  in buildG (0, newVcount) newEdges
\end{code}
} % \hide