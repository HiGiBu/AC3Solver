
\subsection{The Backtracking library}\label{sec:BackTrack}

Using our AC3 instances, we now define a backtracking method to find one or all solutions 
(where possible) for a given instance. 
We start by defining the `output' of our backtracking method, which will be a list \verb:[Assignment a b]:.


% To test this, using the examples from AC3\_Mess:
% \verb: (\m -> findSolution $ AC3 (cons m)  (ac3 m) ): for your chosen example m.

\begin{code}
module Backtracking where

import AC3Solver

type Assignment a b = (Agent a, b)

\end{code}

First of all, we can provide a fast method to check that a solution is even \emph{theoretically} 
possible: if at least 1 agent has an empty domain, then there will never be a legal assignment.

\begin{code}

--Returns true iff at least 1 agent has an empty domain.
--Post: Returns true -> \not \exist a solution. 
--      However, returns false does NOT guarantee that a solution exists.
determineNoSol :: [Domain a b] -> Bool 
determineNoSol = any (\(_,ds) -> null ds) 

\end{code}

Next, we use backtracking to try and find a solution, using backtracking.
For our agent X, we iterate over each value in X's domain. 
For every constraint (X,Y) or (Y,X), where Y has already got an assigned value, 
we check if this constraint holds. If at least one of these constraints does not hold, 
then we continue with the next value in X's domain. Else, we continue with the next agent.
If we find a valid assignment \verb:Just ...:, then we return this, else we try the next 
value in X's domain. 

If no value in X's domain leads to a valid assignment, we return \verb:Nothing:, and try a different
assignment, or return Nothing if no solution exists for this instance.

Notably, while findSolution takes an instance of AC3, we can run \verb:findSolution: \emph{without} 
having run \verb:ac3:, and so we can compare the runtime of findSolution before and after running \verb:ac3:.

\begin{code}

findSolution :: Eq a => AC3 a b -> Maybe [Assignment a b]
findSolution (AC3 c d) = helpFS c d []

helpFS :: Eq a => [ConstraintAA a b] -> [Domain a b] -> [Assignment a b] -> Maybe [Assignment a b]
helpFS _ [] as = Just as -- Done
helpFS constrs ((x, ds):dss) as = recurseFS ds where 
    recurseFS [] = Nothing 
    recurseFS (d:ds') = let 
        -- we want to try assigning value d to agent x. 
        -- Get all constraints (X,Y) and (Y,X), where Y already has a value assigned to it.
        -- Check if x=d works, for all previously assigned values Y.
        checkCons = and $
            [cf d (valY y as) | (x',y,cf)<-constrs, x==x', y `elemAs` as] ++ 
            [cf (valY y as) d | (y,x',cf)<-constrs, x==x', y `elemAs` as]
        in 
        if not checkCons then recurseFS ds' -- easy case, x=d is not allowed.
        else -- 
            case helpFS constrs dss ((x,d):as) of 
                Nothing -> recurseFS ds' -- x=d causes issues later on.
                Just solution -> Just solution -- :)

\end{code}

As with \verb:findSolution:, \verb:findAllSolutions: returns the (possibly empty) list of all solutions, again using backtracking.

\begin{code}

-- find all
findAllSolutions :: Eq a => AC3 a b -> [[Assignment a b]]
findAllSolutions (AC3 c d) = helpFSAll c d []

-- helper function for find all
helpFSAll :: Eq a => [ConstraintAA a b] -> [Domain a b] -> [Assignment a b] -> [[Assignment a b]]
helpFSAll _ [] as = [as]  -- Found a complete solution
helpFSAll constrs ((x, ds):dss) as = concatMap recurseFS ds where
    recurseFS d =
        let checkCons = all (\(x', y, cf) -> not (x == x' && y `elemAs` as) || cf d (valY y as)) constrs
                     && all (\(y, x', cf) -> not (x == x' && y `elemAs` as) || cf (valY y as) d) constrs
        in if checkCons then helpFSAll constrs dss ((x,d):as) else []

\end{code}

Given a solution, verify whether this solution is permissible with the provided constraints.

\begin{code}

checkSolution :: Eq a => [ConstraintAA a b] -> [Assignment a b] -> Bool 
checkSolution [] _ = True 
checkSolution ((x,y,f):cs) as = elemAs x as && elemAs y as && let   
    xN = valY x as 
    yN = valY y as
    in f xN yN && checkSolution cs as

\end{code}

Help-functions used by our solution methods.

\begin{code}

-- Find whether agent Y has an assignment.
elemAs :: Eq a => Agent a -> [Assignment a b] -> Bool 
elemAs _ [] = False
elemAs y ((x,_):as) = x==y || y `elemAs` as 

-- Find agent Y's assigned value
-- PRE: y \in as.
valY :: Eq a => Agent a -> [Assignment a b] -> b 
valY _ [] = error "Y's value could not be found in the assignment." -- should not happen.
valY y ((x,b):as) = if x == y then b else valY y as




\end{code}