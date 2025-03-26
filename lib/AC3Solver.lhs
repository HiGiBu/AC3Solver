\subsection{The AC3Solver library}\label{sec:Solver}

% This section describes a module which we will import later on.
This module contains the main algorithm and definition for our project, the \verb:AC3: type.

\begin{code}
module AC3Solver where

import Control.Monad.Writer
    ( runWriter, MonadWriter(tell), Writer )


\end{code}

To start of, we define the \verb:AC3: instance. 
% For each problem instance, we have a set of variables of type \verb:a:.
An \verb:AC3: instance constains a list of constraints \verb:Arc:, and a list of domains.  
Each \verb:Arc: contains a pair of variables $(X,Y)$, and then a function, such as \verb:(==):, 
which is the constraint on the arc from $X$ to $Y$.
    \footnote{Note that we only allow for \emph{binary} constraints. 
    The AC-3 algorithm does not allow for ternary (or greater) constraints, and unary constraints can be resolved 
    by restricting that variables's domain. \cite{AC3} provides other approaches for achieving path consistency, where you may have ternary (or greater) constraints.}
Each \verb:Domain: item contains an variable, and then a list of values of type \verb:b:. 

We may have multiple constraints for a pair of variables $(X,Y)$, such as both \verb:(>): and \verb:(>=):, 
or an variable may not be in any constraint.  
The programme however expects that each variable has exactly 1 (possibly empty) domain specified for it.

Note that we do not define an arbitrary instance for AC3. Instead, we can define arbitrary instances 
for specific problems. (See for example Section~\ref{sec:GraphCol}.)

\begin{code}

data AC3 a b = AC3 { 
    -- Constraint should take values from the first & second variables as params x & y resp. in \x,y-> x ?=? y. 
    -- We should allow for multiple constraints for (X,Y), eg. both (x > y) AND (x < y) in the set. 
    cons :: [Arc a b], 
    -- Assume we have 1 domain list for each variable.
    domains :: [Domain a b] } 

type Domain a b = (Variable a, [b])
type Arc a b = (Variable a, Variable a, Constraint b)
type Constraint a = a -> a -> Bool

type Variable a = a 

\end{code}

For each constraint $(X,Y,f)$, we want to check for each value $x$ in the domain of $X$ whether 
    there is at least one value $y$ in $Y$'s domain such that \verb:f x y: is satisfied. 
Values of $X$ for which there is no such value in $Y$ are removed from $X$'s domain.
We make use of the Writer monad to do an $O(1)$ lookup to see if we removed items from $X$'s domain.

\begin{code}

-- | Return the elements of xs for which there exist a y \in ys, such that c x y holds.
-- | Using the writer monad, we also give a O(1) method to check whether we altered x's domain after termination.
checkDomain :: [a] -> [a] -> Constraint a -> Writer String [a]
checkDomain [] _ _ = return []
checkDomain (x:xs) ys c = do 
    rest <- checkDomain xs ys c  
    if not $ null [ y' | y'<-ys, c x y'] then return $ x:rest
    else tell "Altered domain" >> return rest -- This is nicely formatted for readability, it could just be something simple such as ".".

\end{code}

\hide{
\begin{code}
-- Then we can use with this with: runWriter $ checkDomain xs ys c
-- newX = fst result, output = snd result. 
-- If output == "", then domain unchanged, and we can simply forget the constraint.
-- Else, look through original set of constraints, and re-add all constraints for ys.
\end{code}
}

Each time we call iterate, we start by looking for the domains of variables $X$ \& $Y$ 
for our constraint $(X,Y)$. Once we find these, we are likely to replace the original
domain for $X$ with a reduced one. We use \verb:popXy: and \verb:popX: to find the domains
for $X$ \& $Y$, and at the same time we also remove the \emph{old} domain for $X$. 
    Using \verb:popXy:, we do one walk through the list, and save two walks, compared to doing a separate lookup for $y$, and 
    a separate walk to delete the old $x$. 
% Once we have checked the current constraint, we then add back the \emph{new} domain for X.

\begin{code}

-- | PRE: x is an element of (a:as) 
-- | POST: Output = (X's domain, the original domain with X's domain removed.)
popX :: Eq a => Variable a -> [Domain a b] -> ([b], [Domain a b] )
popX _ [] = error "No domain found for a Variable X in a constraint." -- should not occur.
popX x (a@(aA, aD):as) = if x == aA then (aD,as) 
                         else let (x', as') = popX x as in (x', a:as')

-- | PRE: x != y; x,y are elements of (a:as). 
-- |     (else, this is not a binary constraint but a unary one.) 
-- | POST: Output = (X's domain [b], Y's domain [b], the original domain d with X's domain removed.)
popXy :: Eq a => Variable a -> Variable a -> [Domain a b] -> ([b], [b], [Domain a b] )
popXy _ _ [] = error "No domains found for both variables X & Y in a constraint." -- should not occur.
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
We get the domains of $X$ \& $Y$, and remove the \emph{old} domains of $X$. 
We then run checkDomain, and add the new domain of $X$ back to the list of domains.
If $X$'s domain was altered, then we add all constraints of the form $(Y,X)$ to the back of the queue.


\begin{code}
-- | Given an legal AC3 instance, this function returns a reduced list of domains, where 
-- |    each variable's domain now only contains values which are arc-consistent with
-- |    the set constraints. 
-- | POST: for each variable x, x's domain in the output \subseteq x's domain originally. 
ac3 :: (Ord a, Ord b) => AC3 a b -> [Domain a b] -- return a list of domains.
ac3 m@(AC3 c d) = let 
    queue = c 
    in iterateAC3 m queue d

iterateAC3 :: (Ord a, Ord b) => AC3 a b -> [Arc a b] -> [Domain a b] 
                -> [Domain a b]
iterateAC3 _ [] d = d
iterateAC3 m@(AC3 fullCS _) ((x,y,c):cs) d = let 
    (xDomain, yDomain, alteredD) = popXy x y d
    (newX, str) = runWriter $ checkDomain xDomain yDomain c
    newDomains = (x, newX) : alteredD
    -- take all constraints of the form (y,x, c)
    z = if null str then cs else cs ++ [c' | c'@(y1,x1,_)<-fullCS, y1/=y && y1/=x, x1==x ] 
    in iterateAC3 m z newDomains 

\end{code}