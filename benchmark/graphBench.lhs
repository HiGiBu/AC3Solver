a
 
\begin{code}

module Main where 

import Criterion.Main

import AC3Solver
import Backtracking
import GraphCol (GraphCol(GC), optimiseGC, readGraphFromFile)

-- ! NOTE: DO NOT IMPORT BOTH AT THE SAME TIME, AS THIS MAY NOT COMPILE.
import GraphColExamples -- File that contains the (very big) GC example instances
--import GraphColExamples2 -- Second file that contains the (very big) GC example instances

-- A method to note the difference before & after running AC3.
getTotalDomainOptions :: [Domain a b] -> Int 
getTotalDomainOptions = foldr (\(_, ds) prev -> length ds + prev) 0

testFiles :: [String] --TODO: Automate this for all files in /lib 
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


runBenchmark :: String -> IO () 
runBenchmark filename = do
  gc@(GC inst) <- readGraphFromFile filename
\end{code}
Run exactly ONE of the following 2 code blocks:
% \begin{code}
%   let origNOpts =  getTotalDomainOptions $ domains inst
%   let newD = ac3 inst  
%   let newNOpts = getTotalDomainOptions newD

%   let (GC optiInst) = optimiseGC gc 
%   let newOptiD = ac3 optiInst
%   let newNOptiD = getTotalDomainOptions newOptiD



%   putStrLn $ "Filename: " ++ filename 
%   putStrLn $ "Pre AC-3:        " ++ show origNOpts
%   putStrLn $ "Post AC-3:       " ++ show newNOpts
%   putStrLn $ "OptimiseGC:      " ++ (show . getTotalDomainOptions . domains) optiInst
%   putStrLn $ "OptimiseGC AC-3: " ++ show newNOptiD
% \end{code}
\begin{code}  
  -- Benchmark Criterion bit
  defaultMain [
    bgroup filename [ bench "pre AC-3" $ whnf findSolution inst
                    , bench "post AC-3" $ whnf findSolution (AC3 (cons inst) (ac3 inst))
                    , bench "OptimiseGC, no AC-3" $ whnf (\(GC oi) -> findSolution oi) (optimiseGC gc)
                    , bench "OptimiseGC, + AC-3 " $ whnf (\(GC oi) -> findSolution (AC3 (cons inst) (ac3 oi))) (optimiseGC gc)
                    ]
    ]


\end{code}




\begin{code}


runBig :: (String, GraphCol) -> IO () 
runBig (sName, gc) = do
  let (GC inst) = gc
  
\end{code}
Run exactly ONE of the following 2 code blocks:
% \begin{code}
%   let origNOpts =  getTotalDomainOptions $ domains inst
%   let newD = ac3 inst  
%   let newNOpts = getTotalDomainOptions newD

%   let (GC optiInst) = optimiseGC gc 
%   let newOptiD = ac3 optiInst
%   let newNOptiD = getTotalDomainOptions newOptiD



%   putStrLn $ "Filename: " ++ sName 
%   putStrLn $ "Pre AC-3:        " ++ show origNOpts
%   putStrLn $ "Post AC-3:       " ++ show newNOpts
%   putStrLn $ "OptimiseGC:      " ++ (show . getTotalDomainOptions . domains) optiInst
%   putStrLn $ "OptimiseGC AC-3: " ++ show newNOptiD
% \end{code}

\begin{code}
  -- Benchmark Criterion bit
  defaultMain [
    bgroup sName [ bench "pre AC-3" $ whnf findSolution inst
                    , bench "post AC-3" $ whnf findSolution (AC3 (cons inst) (ac3 inst))
                    , bench "OptimiseGC, no AC-3" $ whnf (\(GC oi) -> findSolution oi) (optimiseGC gc)
                    , bench "OptimiseGC, + AC-3 " $ whnf (\(GC oi) -> findSolution (AC3 (cons inst) (ac3 oi))) (optimiseGC gc)
                    ]
    ]
\end{code}



\begin{code}

benchmarkTests :: IO ()
benchmarkTests = do 
    --mapM_ runBenchmark $ testFiles ++ testFilesComps ++ testFilesComps3 ++ ["graphcolInstances/n10e40nc3_Neg.txt"]
    mapM_ runBig ac3_Proof_Examples
    --mapM_ runBig ac3_duplicatedGraph

main :: IO ()
main = benchmarkTests

\end{code}

