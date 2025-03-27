\hide{
\begin{code}
module ZebraPuzzle where
import AC3Solver
\end{code}}
\subsection{Zebra Puzzle}
Other types of problems can be solved using the AC3 algorithm. The Zebra Puzzle, a well known logic puzzle shown below, is an example:
\begin{quote}
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
\end{quote}
\begin{code}
houses = ["1", "2", "3", "4", "5"]
nationalities = ["Englishman", "Spaniard", "Ukrainian", "Norwegian", "Japanese"]
colors = ["red", "green", "ivory", "yellow", "blue"]
drinks = ["coffee", "tea", "milk", "orange juice", "water"]
smokes = ["Old Gold", "Kools", "Chesterfields", "Lucky Strike", "Parliaments"]
pets = ["dog", "snails", "fox", "horse", "zebra"]
agents = houses ++ nationalities ++ colors ++ drinks ++ smokes ++ pets


permute6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
permute6 al bl cl dl el fl = [(a, b, c, d, e, f) | a <- al, b <- bl, c <- cl, d <- dl, e <- el, f <- fl]

domainH = [(h, permute6 [h] colors drinks smokes pets nationalities) | h <- houses]
domainC = [(c, permute6 houses [c] drinks smokes pets nationalities) | c <- colors]
domainD = [(dr, permute6 houses colors [dr] smokes pets nationalities) | dr <- drinks] 
domainS = [(s, permute6 houses colors drinks [s] pets nationalities) | s <- smokes]
domainP = [(p, permute6 houses colors drinks smokes [p] nationalities) | p <- pets]
domainN = [(n, permute6 houses colors drinks smokes pets [n]) | n <- nationalities]
d = domainH ++ domainC ++ domainD ++ domainS ++ domainP ++ domainN

\end{code}

\begin{code}
constraintVal = [(x, y, validity) | x <- agents, y <- agents, x /= y]
ints :: Eq a => [a] -> [a] -> [a]
ints xs ys = filter (`elem` ys) xs
validity :: (String, String, String, String, String, String) ->  (String, String, String, String, String, String) -> Bool
validity (h, c, dr, s, p, n) _ = (null eRint || eRint == eR) && (null sDint || sDint == sD) && (null cGint || cGint == cG) && (null uTint || uTint == uT) && (null oGint || oGint == oG) && (null kYint || kYint == kY) && (null m3int || m3int == m3) && (null n1int || n1int == n1) && (null lSint || lSint == lS) && (null jPint || jPint == jP)
        where 
        eRint = ints eR [h, c, dr, s, p, n]
        sDint = ints sD [h, c, dr, s, p, n]
        cGint = ints cG [h, c, dr, s, p, n]
        uTint = ints uT [h, c, dr, s, p, n]
        oGint = ints oG [h, c, dr, s, p, n]
        kYint = ints kY [h, c, dr, s, p, n]
        m3int = ints m3 [h, c, dr, s, p, n]
        n1int = ints n1 [h, c, dr, s, p, n]
        lSint = ints lS [h, c, dr, s, p, n]
        jPint = ints jP [h, c, dr, s, p, n]
cF = ["Chesterfield", "fox"]
kH = ["Kools", "horse"]
nB = ["Norwegian", "blue"]
gi = ["green", "ivory"]
constraintValN :: [(String, String,   (String, String, String, String, String, String)   -> (String, String, String, String, String, String) -> Bool)]
constraintValN = [(x, y, validityN) | x <- agents, y <- agents, x /= y]
validityN :: (String, String, String, String, String, String) ->  (String, String, String, String, String, String) -> Bool
validityN (h, c, dr, s, p, n) _ = cFint /= cF && kHint /= kH && nBint /= nB && giint /= gi
        where 
        cFint = ints cF [h, c, dr, s, p, n]
        kHint = ints kH [h, c, dr, s, p, n]
        nBint = ints nB [h, c, dr, s, p, n]
        giint = ints gi [h, c, dr, s, p, n]

notEqual :: (String, String, String, String, String, String) ->  (String, String, String, String, String, String) -> Bool
notEqual (a, b, c, d, e, f) (g, h, i, j, k, l) = a /= g && b /= h && c /= i && d /= j && e /= k && f /= l

unique :: (String, String, String, String, String, String) -> (String, String, String, String, String, String) -> Bool  
unique w@(h, c, dr, s, p, n) w'@(h', c', dr', s', p', n') = (w == w') || (h /= h' && c /= c' && dr /= dr' && s /= s' && p /= p' && n /= n')
checkDistance :: String -> String  -> Int
checkDistance x y = abs (read x -  read y)
greenIvory :: (String, String, String, String, String, String) ->  (String, String, String, String, String, String) -> Bool
greenIvory (x, _, _, _, _, _) (y, _, _, _, _, _) = (x, y) == ("1", "2") || (x, y) == ("2", "3") || (x, y) == ("3","4") || (x, y) == ("4", "5")

nextDoor :: (String, String, String, String, String, String) ->  (String, String, String, String, String, String) -> Bool
nextDoor (x, _, _, _, _, _) (y, _, _, _, _, _) = checkDistance x y == 1



eR = ["Englishman", "red"]
sD = ["Spaniard", "dog"]
cG = ["coffee", "green"]
uT = ["Ukrainian", "tea"]
oG = ["Old Gold", "snails"]
kY = ["Kools", "yellow"]
m3 = ["milk", "3"]
n1 = ["Norwegian", "1"]
lS = ["Lucky Strike", "orange juice"]
jP = ["Japanese", "Parliaments"]

constraintEr = [("Englishman", "red", (==)), ("red", "Englishman", (==))]
constraintSd = [("Spaniard", "dog", (==)), ("dog", "Spaniard", (==))] 
constraintCg = [("coffee", "green", (==)), ("green", "coffee", (==))] 
constraintUt = [("Ukrainian", "tea", (==)), ("tea", "Ukrainian", (==))] 
constraintOg = [("Old Gold", "snails", (==)), ("snails", "Old Gold", (==))] 
constraintKy = [("Kools", "yellow", (==)), ("yellow", "Kools", (==))] 
constraintM3 = [("milk", "3", (==)), ("3", "milk", (==))] 
constraintN1 = [("Norwegian", "1", (==)), ("1", "Norwegian", (==))] 
constraintLs = [("Lucky Strike", "orange juice", (==)), ("orange juice", "Lucky Strike", (==))] 
constraintJp = [("Japanese", "Parliaments", (==)), ("Parliaments", "Japanese", (==))]
constraintEq = constraintEr ++ constraintSd ++ constraintCg ++ constraintUt ++ constraintOg ++ constraintKy ++ constraintM3 ++ constraintN1 ++ constraintLs ++ constraintJp
constraintUnique = [(x, y, unique) | x <- agents, y <- agents, x /= y]
constraintGi = [("green", "ivory", greenIvory), ("ivory", "green", flip greenIvory), ("green", "ivory", notEqual)]
constraintCf = [("Chesterfields", "fox", nextDoor), ("fox", "Chesterfields", nextDoor), ("Chesterfields", "fox", notEqual)]
constraintKh = [("Kools", "horse", nextDoor), ("horse", "Kools", nextDoor), ("Kools", "horse", notEqual)]
constraintNb = [("Norwegian", "blue", nextDoor), ("blue", "Norwegian", nextDoor), ("Norwegian", "blue", notEqual)]
constraintsNeQ = constraintGi ++ constraintCf ++ constraintKh ++ constraintNb

constraints = constraintEq ++ constraintsNeQ ++ constraintUnique
\end{code}

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