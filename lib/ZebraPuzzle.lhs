\hide{
\begin{code}
module ZebraPuzzle where
import AC3Solver
\end{code}}
\subsection{Zebra Puzzle}
Other types of problems can be solved using the AC3 algorithm. The Zebra Puzzle, a well known logic puzzle shown below, is an example:
\begin{verbatim}
There are five houses.
The Englishman lives in the red house.
The Spaniard owns the dog.
Coffee is drunk in the green house.
The Ukrainian drinks tea.
The Old Gold smoker owns snails.
Kools are smoked in the yellow house.
Milk is drunk in the middle house.
The Norwegian lives in the first house.
The Lucky Strike smoker drinks orange juice.
The Japanese smokes Parliaments.
The green house is immediately to the right of the ivory house.
The man who smokes Chesterfields lives in the house next to the man with the fox.
Kools are smoked in the house next to the house where the horse is kept.
The Norwegian lives next to the blue house
\end{verbatim}
Setting up the variables and the domain.
\begin{code}
houses :: [String]
houses = ["1", "2", "3", "4", "5"]
nationalities :: [String]
nationalities = ["Englishman", "Spaniard", "Ukrainian", "Norwegian", "Japanese"]
colors :: [String]
colors = ["red", "green", "ivory", "yellow", "blue"]
drinks :: [String]
drinks = ["coffee", "tea", "milk", "orange juice", "water"]
smokes :: [String]
smokes = ["Old Gold", "Kools", "Chesterfields", "Lucky Strike", "Parliaments"]
pets :: [String]
pets = ["dog", "snails", "fox", "horse", "zebra"]
agents :: [String]
agents = houses ++ nationalities ++ colors ++ drinks ++ smokes ++ pets
type World = (String, String, String, String, String, String)
type Var = String
permute6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
permute6 hl cl drl sl pl nl = [(h, c, dr, s, p, n) | h <- hl, c <- cl, dr <- drl, s <- sl, p <- pl, n <- nl]

domainH :: [(Var, [World])]
domainH = [(h, permute6 [h] colors drinks smokes pets nationalities) | h <- houses]
domainC :: [(String, [World])]
domainC = [(c, permute6 houses [c] drinks smokes pets nationalities) | c <- colors]
domainD :: [(String, [World])]
domainD = [(dr, permute6 houses colors [dr] smokes pets nationalities) | dr <- drinks] 
domainS :: [(String, [World])]
domainS = [(s, permute6 houses colors drinks [s] pets nationalities) | s <- smokes]
domainP :: [(String, [World])]
domainP = [(p, permute6 houses colors drinks smokes [p] nationalities) | p <- pets]
domainN :: [(String, [World])]
domainN = [(n, permute6 houses colors drinks smokes pets [n]) | n <- nationalities]
d :: [(String, [World])]
d = domainH ++ domainC ++ domainD ++ domainS ++ domainP ++ domainN

\end{code}

\hide{
\begin{code}
ints :: Eq a => [a] -> [a] -> [a]
ints xs ys = filter (`elem` ys) xs

notEqual :: World ->  World -> Bool
notEqual (h, c, dr, s, p, n) (h', c', dr', s', p', n') = h /= h' && c /= c' && dr /= dr' && s /= s' && p /= p' && n /= n'

unique :: World -> World -> Bool  
unique w@(h, c, dr, s, p, n) w'@(h', c', dr', s', p', n') = (w == w') || (h /= h' && c /= c' && dr /= dr' && s /= s' && p /= p' && n /= n')
checkDistance :: String -> String  -> Int
checkDistance x y = abs (read x -  read y)
greenIvory :: World ->  World -> Bool
greenIvory (x, _, _, _, _, _) (y, _, _, _, _, _) = (x, y) == ("1", "2") || (x, y) == ("2", "3") || (x, y) == ("3","4") || (x, y) == ("4", "5")

nextDoor :: World ->  World -> Bool
nextDoor (x, _, _, _, _, _) (y, _, _, _, _, _) = checkDistance x y == 1
\end{code}}

Only keep the valid parts of the domain. (This is cheating a little as it is not really arc consistency, but it is using that program.)
\begin{code}
nonEqualPairs :: [[String]]
nonEqualPairs = [["Chesterfields", "fox"], ["Kools", "horse"], ["Norwegian", "blue"], ["green", "ivory"]]

equalPairs :: [[String]]
equalPairs = [["Englishman", "red"], ["Spaniard", "dog"], ["coffee", "green"], ["Ukrainian", "tea"], ["Old Gold", "snails"], ["Kools", "yellow"], ["milk", "3"], ["Norwegian", "1"], ["Lucky Strike", "orange juice"], ["Japanese", "Parliaments"]]

validity :: World ->  World -> Bool
validity (h, c, dr, s, p, n) _ =   all ((\ x -> null x || (length x == 2)) . (`ints` [h, c, dr, s, p, n])) equalPairs
validityN :: World ->  World -> Bool
validityN (h, c, dr, s, p, n) _ =   all ((\ x -> length x /= 2) . (`ints` [h, c, dr, s, p, n])) nonEqualPairs


constraintVal :: [(String, String,   World  -> World -> Bool)]
constraintVal = [(x, y, validity) | x <- agents, y <- agents, x /= y]

constraintValN :: [(String, String,   World   -> World -> Bool)]
constraintValN = [(x, y, validityN) | x <- agents, y <- agents, x /= y]
\end{code}
Constraints for equality, uniqueness, and nonequality.
\begin{code}
constraintEq :: [(String, String,   World   -> World -> Bool)]
constraintEq = [(x, y, (==)) | [x, y] <- equalPairs] ++ [(x, y, (==)) | [y, x] <- equalPairs]
constraintUnique :: [(String, String, World -> World -> Bool)]
constraintUnique = [(x, y, unique) | x <- agents, y <- agents, x /= y]
constraintGi :: [(String, String, World -> World -> Bool)]
constraintGi = [("green", "ivory", greenIvory), ("ivory", "green", flip greenIvory)]
constraintCf :: [(String, String, World -> World -> Bool)]
constraintCf = [("Chesterfields", "fox", nextDoor), ("fox", "Chesterfields", nextDoor)]
constraintKh :: [(String, String, World -> World -> Bool)]
constraintKh = [("Kools", "horse", nextDoor), ("horse", "Kools", nextDoor)]
constraintNb :: [(String, String, World -> World -> Bool)]
constraintNb = [("Norwegian", "blue", nextDoor), ("blue", "Norwegian", nextDoor)]
constraintsNeQ :: [(String, String, World -> World -> Bool)]
constraintsNeQ = [(x, y, notEqual) | [x, y] <- nonEqualPairs] ++ [(x, y, notEqual) | [y, x] <- nonEqualPairs] ++ constraintGi ++ constraintCf ++ constraintKh ++ constraintNb

constraints :: [(String, String, World -> World -> Bool)]
constraints = constraintEq ++ constraintsNeQ ++ constraintUnique
\end{code}
Finally running the code to solve the puzzle.
\begin{code}

zebraPuzzleMain :: IO ()
zebraPuzzleMain = do
    let validDomain = ac3 (AC3 (constraintVal ++ constraintValN) d)
    let ac3Puzzle = AC3 constraints validDomain
    let ac3solved = ac3 ac3Puzzle
    let solution = ac3solved
    if null solution
    then putStrLn "No solution found."
    else do
        putStrLn "Solution: "
        print solution
\end{code}