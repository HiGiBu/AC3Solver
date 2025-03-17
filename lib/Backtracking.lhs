
\section{The BackProp library}\label{sec:BackProp}

This section describes a module which we will import later on.

To test this, using the examples from AC3\_Mess:
\verb: (\m -> findSolution $ AC3 (cons m)  (ac3 m) ): for your chosen example m.


\begin{code}
module Backtracking where

import AC3Solver

type Assignment a b = (Agent a, b)

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


-- Find whether agent Y has an assignment.
elemAs :: Eq a => Agent a -> [Assignment a b] -> Bool 
elemAs _ [] = False
elemAs y ((x,_):as) = x==y || y `elemAs` as 

-- Find agent Y's assigned value
-- PRE: y \in as.
valY :: Eq a => Agent a -> [Assignment a b] -> b 
valY _ [] = undefined -- should not happen.
valY y ((x,b):as) = if x == y then b else valY y as


\end{code}