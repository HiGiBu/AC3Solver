\section{The AC3Solver library}\label{sec:Solver}

This section describes a module which we will import later on.

\begin{code}
module AC3Solver where

import Control.Monad.Writer
    ( runWriter, MonadWriter(tell), Writer )


\end{code}

To start of, we define the AC3 instance. For each agent, we have a set of agents of type \verb:a:.
An AC3 instance then constains a list of constraints \verb:constraintAA:, and a list of domains. 
Each \verb:constraintAA: contains a pair of agents (X,Y), and then a function, such as \verb:(==):, 
    which is the constraint on the arc from X to Y. 
Each \verb:Domain: item contains an agent, and then a list of values of type \verb:b:. 

We may have multiple constraints for a pair of agentsd (X,Y), such as both \verb:(>): and \verb:(>=):. 
The programme however expects that each agent has exactly 1 domain specified for it, and 
each constraint (X,Y) should have some (possibly empty) domain assigned to it.

\begin{code}

data AC3 a b = AC3 { 
    -- Constraint should take values from the first & second agents as params x & y resp. in \x,y-> x ?=? y. 
    -- We should allow for multiple constraints for (X,Y), eg. both (x > y) AND (x < y) in the set. 
    cons :: [ConstraintAA a b], 
    -- Assume we have 1 domain list for each variable. (TODO: Check for this? )
    domains :: [Domain a b] } 

type Domain a b = (Agent a, [b])
type ConstraintAA a b = (Agent a, Agent a, Constraint b)
type Constraint a = a -> a -> Bool

type Agent a = a 

\end{code}

For each constraint (X,Y,f), we want to check for each value in the domain of X whether 
    there is a value in Y's domain such that \verb:f x y: is satisfied. 
Values of X for which there is no such value in Y are removed from X's domain.
We make use of the Writer monad to do an $O(1)$ lookup to see if we removed items from X's domain.

\begin{code}

-- Return the elements of xs for which there exist a y \in ys, such that c x y holds.
-- Using the writer monad, we also give a O(1) method to check whether we altered x's domain after termination.
checkDomain :: [a] -> [a] -> Constraint a -> Writer String [a]
checkDomain [] _ _ = return []
checkDomain (x:xs) ys c = do 
    rest <- checkDomain xs ys c  
    if not $ null [ y' | y'<-ys, c x y'] then return $ x:rest
    else tell "Altered domain" >> return rest -- This is nicely formatted for readability, it could just be "." or whatever.

\end{code}

\hide{
\begin{code}
-- Then we can use with this with: runWriter $ checkDomain xs ys c
-- newX = fst result, output = snd result. 
-- If output == "", then domain unchanged, and we can simply forget the constraint.
-- Else, look through original set of constraints, and re-add all constraints for ys.
\end{code}
}

Each time we call iterate, we start of by looking for the domains of agents X & Y 
for our constraint (X,Y). Once we find these, we are likely to replace the original
domain for X with a reduced one. We use \verb:popXy: and \verb:popX: to find the domains
for X & Y, and at the same time we also remove the \emph{old} domain for X. 
    Using this is 1 walk through the list, and saves us 2 walks. (Separate lookup for y, and 
    a walk to delete the old x.)
Once we have checked the current constraint, we then add back the \emph{new} domain for X.

\begin{code}

-- PRE: x is an element of (a:as) 
popX :: Eq a => Agent a -> [Domain a b] -> ([b], [Domain a b] )
popX _ [] = undefined -- should not occur.
popX x (a@(aA, aD):as) = if x == aA then (aD,as) else let (x', as') = popX x as in (x', a:as')

-- PRE: x != y; x,y are elements of (a:as). 
--      (else, this is not a binary constraint but a unary one.) 
popXy :: Eq a => Agent a -> Agent a -> [Domain a b] -> ([b], [b], [Domain a b] )
popXy _ _ [] = undefined -- should not occur.
popXy x y (a@(aA, aD):as) 
    | x == aA = let -- we want to REMOVE a from the list.
        -- search through the rest of the list and return y's domain.
        yDomain = head [b' | (a',b')<-as, y==a' ]
        in (aD, yDomain, as)
    | y == aA = let (retX, retAs) = popX x as in (retX, aD, a:retAs)
    | otherwise = let (retX, retY, retAs) = popXy x y as in (retX, retY, a:retAs)


\end{code}

We now come to the main part of the algorithm. The iterateAC3 function runs as long as the 
    queue of constraints is not empty, starting with the original set of constraints. 
We get the domains of X & Y, and remove the \emph{old} domains of X. 
We then run checkDomain, and add the new domain of X back to the list of domains.
If X's domain was altered, then we add all constraints of the form (Y,X) to the back of the queue.


\begin{code}

ac3 :: (Ord a, Ord b) => AC3 a b -> [Domain a b] -- return a list of domains.
ac3 m@(AC3 c d) = let 
    queue = c -- put each constraint into the queue.  -- TODO: implement this better, eg a priority queue?
    in iterateAC3 m queue d

iterateAC3 :: (Ord a, Ord b) => AC3 a b -> [ConstraintAA a b] -> [Domain a b] -> [Domain a b]
iterateAC3 _ [] d = d
iterateAC3 m@(AC3 fullCS _) ((x,y,c):cs) d = let 
    (xDomain, yDomain, alteredD) = popXy x y d
    (newX, str) = runWriter $ checkDomain xDomain yDomain c
        -- In a lens, we could do this with "modify (\ (a,_) -> (a, newX))"
    newDomains = (x, newX) : alteredD
    z = if null str then cs else cs ++ [c' | c'@(y1,x1,_)<-fullCS, y1==y, x1==x ] -- take all constraints of the form (y,x, c)
    in iterateAC3 m z newDomains 

\end{code}