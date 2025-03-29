\subsection{The Scheduling library}\label{sec:Scheduling}

This module uses the AC3 algorithm to solve timetable scheduling problems. The user can input the variables and contraints by using either 
a parser or loading the problem from a text file. The programme then prints a day, room and time assignment for each course that needed to 
be scheduled.

\hide{
\begin{code}

module Scheduling where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (replicateM)
import Data.List
import AC3Solver
import Backtracking (findSolution)
import Data.Char (toLower)

type ClassAssignment = (Int, Int, Int)
dayNames :: [String]
dayNames = ["monday", "tuesday", "wednesday", "thursday", "friday"]
\end{code}}

The choice of input method is made by setting the variable filePath to either "Nothing" for the parser or to a "Just [string]" containing 
the path of the input file. % Andy: Added the [], and removed the cap, to make it clear we don't mean the *type* String.

\begin{code}
filePath :: Maybe String
filePath = Nothing
--filePath = Just "filePath"
\end{code}

If the user chooses to use the parser, then they will be prompted to type both the total number and names of the given courses, rooms and time slots.
Moreover, it is assumed that the courses can be scheduled Monday to Friday, and that each class will occupy one time slot.

\begin{code}
parseInt :: Parser Int
parseInt = do
  spaces
  n <- many1 digit
  return (read n)
  
parseInput :: String -> IO Int
parseInput prompt = do
  putStrLn prompt
  read <$> getLine

getNames :: String -> Int -> IO [String]
getNames prompt n = do
  putStrLn prompt
  map (map toLower) <$> replicateM n getLine

getUserInputs :: IO (Int, Int, Int, [String], [String], [String])
getUserInputs = do
  numClasses <- parseInput "Enter the number of classes:"
  classNames <- getNames "Enter class names:" numClasses
  numRooms <- parseInput "Enter the number of rooms:"
  roomNames <- getNames "Enter room names:" numRooms
  numTimeSlots <- parseInput "Enter the number of time slots per day:"
  timeSlotNames <- getNames "Enter time slot names:" numTimeSlots
  return (numClasses, numRooms, numTimeSlots, classNames, roomNames, timeSlotNames)
\end{code}

If the user loads the problem from a text file, then the programme will immediately return a solution without user prompts. 
The file needs to contain two delimiters to seperate the variable names from the constraints and starting values. 
A valid input file might look like this:
\begin{verbatim}
class1 class2 class3 class4
room1 room2 room3 room4
9am 10am 11am 12pm 1pm
--- CONSTRAINTS ---
class1 is before class2
class2 is at the same time as class3
class3 is in the same room as class4
class1 is not the same day as class3
class2 is not at the same time as class4
class4 is not in the same room as class1
--- STARTING VALUES ---
class1 is in room1
class2 is at 10am
class3 is on monday
class4 is in room4
\end{verbatim}

\begin{code}
readTextFile :: FilePath -> IO ([String], [String], [String], [String], [String])
readTextFile path = do
  content <- readFile path
  let linesOfFile = filter (not . null) (lines content)

  let (headerLines, rest1) = break (isPrefixOf "--- CONSTRAINTS ---") linesOfFile
  let (constraintLines, rest2) = break (isPrefixOf "--- STARTING VALUES ---") (tail rest1)

  if null headerLines || null rest1 || null constraintLines || null rest2
    then error "Invalid file format."
    else case headerLines of
      [classNamesLine, roomNamesLine, timeSlotNamesLine] -> do
        let classNames = words classNamesLine
        let roomNames = words roomNamesLine
        let timeSlotNames = words timeSlotNamesLine
        return (classNames, roomNames, timeSlotNames, constraintLines, tail rest2)
      _ -> error "Invalid header format."
\end{code}

\hide{
\begin{code}
testUserInputs :: IO ()
testUserInputs = do
  (numClasses, numRooms, numTimeSlots, classNames, roomNames, timeSlotNames) <- getUserInputs
  
  putStrLn "\nCollected Inputs:"
  putStrLn $ "Number of Classes: " ++ show numClasses
  putStrLn $ "Class Names: " ++ show classNames
  putStrLn $ "Number of Rooms: " ++ show numRooms
  putStrLn $ "Room Names: " ++ show roomNames
  putStrLn $ "Number of Time Slots per Day: " ++ show numTimeSlots
  putStrLn $ "Time Slot Names: " ++ show timeSlotNames

checkSameDay :: ClassAssignment -> ClassAssignment -> Bool
checkSameDay (x,_,_) (y,_,_) = x == y

checkSameRoom :: ClassAssignment -> ClassAssignment -> Bool
checkSameRoom (_,_,x) (_,_,y) = x == y

checkSameTime :: ClassAssignment -> ClassAssignment -> Bool
checkSameTime (_,x,_) (_,y,_) = x == y

checkNotSameDay :: ClassAssignment -> ClassAssignment -> Bool
checkNotSameDay (x,_,_) (y,_,_) = x /= y

checkNotSameRoom :: ClassAssignment -> ClassAssignment -> Bool
checkNotSameRoom (_,_,x) (_,_,y) = x /= y

checkNotSameTime :: ClassAssignment -> ClassAssignment -> Bool
checkNotSameTime (_,x,_) (_,y,_) = x /= y

checkBefore :: ClassAssignment -> ClassAssignment -> Bool
checkBefore (x,x2,_) (y,y2,_) = x == y && x2 + 1 == y2

checkAfter :: ClassAssignment -> ClassAssignment -> Bool
checkAfter (x,x2,_) (y,y2,_) = x == y && x2 == y2 + 1

checkDay :: Int -> ClassAssignment -> Bool
checkDay a (x,_,_) = x == a

checkTime :: Int -> ClassAssignment -> Bool
checkTime a (_,x,_) = x == a

checkRoom :: Int -> ClassAssignment -> Bool
checkRoom a (_,_,x) = x == a

filterDomains :: [Domain Int ClassAssignment] -> [(Variable Int, ClassAssignment -> Bool)] -> [Domain Int ClassAssignment]
filterDomains domainList conditions =
    [(agent, [v | v <- values, all (\(a, f) -> (a /= agent) || f v) conditions]) | (agent, values) <- domainList]
\end{code}
}

The parser for the constraints runs until the user types "Done". Possible constraints are: "is the same day as",
"is in the same room as", "is at the same time as" and the corresponding negations. Additionally there is "is before", which defines
that two classes are in two adjacent time slots on the same day.

\begin{code}
getConstraint :: [String] -> IO (Maybe [Arc Int ClassAssignment])
getConstraint classNames = do
  putStrLn "Enter a constraint (e.g., 'class1 is before class2', 'class1 is the same day as class2', 'class1 is in the same room as class2' or 'class1 is not at the same time as class2'). Type 'Done' to finish:"
  input <- getLine
  let parts = map (map toLower) (words input)
  if input == "Done" || input == "done" then return Nothing else do
    case parts of
      [class1, "is", "before", class2] -> Just <$> processConstraints class1 class2 "is before" classNames
      [class1, "is", "the", "same", "day", "as", class2] -> Just <$> processConstraints class1 class2 "is the same day as" classNames
      [class1, "is", "at", "the", "same", "time", "as", class2] -> Just <$> processConstraints class1 class2 "is at the same time as" classNames
      [class1, "is", "in", "the", "same", "room", "as", class2] -> Just <$> processConstraints class1 class2 "is in the same room as" classNames
      [class1, "is", "not", "the", "same", "day", "as", class2] -> Just <$> processConstraints class1 class2 "is not the same day as" classNames
      [class1, "is", "not", "at", "the", "same", "time", "as", class2] -> Just <$> processConstraints class1 class2 "is not at the same time as" classNames
      [class1, "is", "not", "in", "the", "same", "room", "as", class2] -> Just <$> processConstraints class1 class2 "is not in the same room as" classNames
      _ -> do
        putStrLn "Invalid input format"
        getConstraint classNames

processConstraints :: String -> String -> String -> [String] -> IO [Arc Int ClassAssignment]
processConstraints class1 class2 keyword classNames = do
  let class1' = map toLower class1
      class2' = map toLower class2
  case (elemIndex class1' (map (map toLower) classNames), elemIndex class2' (map (map toLower) classNames)) of
    (Just i, Just j) -> case keyword of
      "is before" -> return [(i, j, checkBefore), (j, i, checkAfter)]
      "is the same day as"  -> return [(i, j, checkSameDay), (j, i, checkSameDay)]
      "is in the same room as"  -> return [(i, j, checkSameRoom), (j, i, checkSameRoom)]
      "is at the same time as"  -> return [(i, j, checkSameTime), (j, i, checkSameTime)]
      "is not the same day as"  -> return [(i, j, checkNotSameDay), (j, i, checkNotSameDay)]
      "is not in the same room as"  -> return [(i, j, checkNotSameRoom), (j, i, checkNotSameRoom)]
      "is not at the same time as"  -> return [(i, j, checkNotSameTime), (j, i, checkNotSameTime)]
      _           -> error "Invalid keyword"
    _ -> error "Invalid class names"

collectConstraints :: [String] -> IO [Arc Int ClassAssignment]
collectConstraints classNames = do
  let loop acc = do
        constraint <- getConstraint classNames
        case constraint of
          Nothing -> return acc
          Just cs  -> loop (cs ++ acc)
  loop []
\end{code}

The same constraints can also be loaded from file. They have to be preceded by the delimiter "--- CONSTRAINTS ---".

\begin{code}
getFileConstraints :: [String] -> [String] -> [String] -> [String] -> IO [Arc Int ClassAssignment]
getFileConstraints classNames _ _ constraints = do
    let processLine line = case words line of
            [class1, "is", "before", class2] -> processConstraints class1 class2 "is before" classNames
            [class1, "is", "the", "same", "day", "as", class2] -> processConstraints class1 class2 "is the same day as" classNames
            [class1, "is", "at", "the", "same", "time", "as", class2] -> processConstraints class1 class2 "is at the same time as" classNames
            [class1, "is", "in", "the", "same", "room", "as", class2] -> processConstraints class1 class2 "is in the same room as" classNames
            [class1, "is", "not", "the", "same", "day", "as", class2] -> processConstraints class1 class2 "is not the same day as" classNames
            [class1, "is", "not", "at", "the", "same", "time", "as", class2] -> processConstraints class1 class2 "is not at the same time as" classNames
            [class1, "is", "not", "in", "the", "same", "room", "as", class2] -> processConstraints class1 class2 "is not in the same room as" classNames
            _ -> error "Invalid constraint format"
    concat <$> mapM processLine constraints
\end{code}

The user can also enter any room, time and day values that are already known. The possible keywords are: "is in", "is at" and "is on".
% Andy: What does this also refer to?

\begin{code}
getStartingValues :: [String] -> [String] -> [String] -> IO (Maybe (Variable Int, ClassAssignment -> Bool))
getStartingValues classNames roomNames timeslotNames = do
  putStrLn "Enter known values (e.g., 'class1 is in room3', 'class1 is at 11am' or 'class1 is on monday'). Type 'Done' to finish:"
  input <- getLine
  if input == "Done" || input == "done" then return Nothing else do
    let parts = words input
    case parts of
      [class1, "is", "in", room] -> Just <$> processStartingValues class1 room "is in" classNames roomNames
      [class1, "is", "at", time] -> Just <$> processStartingValues class1 time "is at" classNames timeslotNames
      [class1, "is", "on", day] -> Just <$> processStartingValues class1 day "is on" classNames dayNames
      _ -> do
        putStrLn "Invalid input format"
        getStartingValues classNames roomNames timeslotNames

processStartingValues :: String -> String -> String -> [String] -> [String] -> IO (Variable Int, ClassAssignment -> Bool)
processStartingValues class1 value keyword classNames valueNames = do
  case (elemIndex class1 classNames, elemIndex value valueNames) of
    (Just i, Just j) -> case keyword of
      "is in" -> return (i, checkRoom j)
      "is at" -> return (i, checkTime j)
      "is on" -> return (i, checkDay j)
      _           -> error "Invalid keyword"
    _ -> error "Invalid name"

collectStartingValues :: [String] -> [String] -> [String] -> IO [(Variable Int, ClassAssignment -> Bool)]
collectStartingValues classNames roomNames timeslotNames = do
  let loop acc = do
        value <- getStartingValues classNames roomNames timeslotNames
        case value of
          Nothing -> return acc
          Just svs  -> loop (svs : acc)
  loop []
\end{code}

If the starting values are loaded from file, then they have to be preceded by the delimiter "--- STARTING VALUES ---".

\begin{code}
getFileStartingValues :: [String] -> [String] -> [String] -> [String] -> IO [(Variable Int, ClassAssignment -> Bool)]
getFileStartingValues classNames roomNames timeslotNames startValues = do
  let processLine line = case words line of
            [class1, "is", "in", room] -> processStartingValuesFile class1 room "is in" classNames roomNames
            [class1, "is", "at", time] -> processStartingValuesFile class1 time "is at" classNames timeslotNames
            [class1, "is", "on", day] -> processStartingValuesFile class1 day "is on" classNames dayNames
            _ -> error "Invalid starting value format"
  concat <$> mapM processLine startValues

processStartingValuesFile :: String -> String -> String -> [String] -> [String] -> IO [(Variable Int, ClassAssignment -> Bool)]
processStartingValuesFile class1 value keyword classNames valueNames = do
  case (elemIndex class1 classNames, elemIndex value valueNames) of
    (Just i, Just j) -> case keyword of
      "is in" -> return [(i, checkRoom j)]
      "is at" -> return [(i, checkTime j)]
      "is on" -> return [(i, checkDay j)]
      _ -> error "Invalid keyword"
    _ -> error "Invalid name"
\end{code}

\hide{
\begin{code}
printSolution :: [String] -> [String] -> [String] -> [String] -> [(Variable Int, ClassAssignment)] -> IO ()
printSolution classNames days roomNames timeSlotNames list = putStrLn $ concat
  [classNames !! agent ++ " is scheduled on " ++ days !! dayId ++
    " in " ++ roomNames !! roomId ++ " at " ++ timeSlotNames !! timeId ++ ".\n"
  | (agent, (dayId, timeId, roomId)) <- list]
\end{code}
}

The main function collects all the variables and constraints. Additionally, it adds a uniqueness constraints to make sure that
no two courses are in the same room at the same time. 
% Andy: which function? (Left blank) -> I have *guessed* at filteredDomains
The \verb:filteredDomains: function then generates the possible domains for all variables and filters
them by the known values that the user entered. 
The filtered domains and constraints are passed to the AC-3 algorithm which reduces
the domains. 
Finally, backtracking is used to either find a possible solution or output an error message if no solution exists.

\begin{code}
schedulingMain :: IO ()
schedulingMain = case filePath of 
  Nothing -> do
    (numClasses, numRooms, numTimeSlots, classNames, roomNames, timeSlotNames) <- getUserInputs
    constraints <- collectConstraints classNames
    let uniquenessConstraints = [(i, j, (/=)) | i <- [0..numClasses-1], j <- [0..numClasses-1], i /= j]
    let allConstraints = constraints ++ uniquenessConstraints
    let classDomains = [(i, [(d, t, r) | d <- [0..5], t <- [0..numTimeSlots-1], r <- [0..numRooms-1]]) | i <- [0..numClasses-1]]
    domainConditions <- collectStartingValues classNames roomNames timeSlotNames
    let filteredDomains = filterDomains classDomains domainConditions
    let possibleSolutions = ac3 AC3 { cons = allConstraints, domains = filteredDomains }
    let solution = findSolution AC3 { cons = allConstraints, domains = possibleSolutions }
    case solution of
      Nothing -> putStrLn "No solution found."
      Just sol -> printSolution classNames dayNames roomNames timeSlotNames sol

  Just path -> do
    (classNames, roomNames, timeSlotNames, fileConstraints, fileStartingValues) <- readTextFile path
    constraints <- getFileConstraints classNames roomNames timeSlotNames fileConstraints
    startingValues <- getFileStartingValues classNames roomNames timeSlotNames fileStartingValues
    let numClasses = length classNames
    let numRooms = length roomNames
    let numTimeSlots = length timeSlotNames
    let uniquenessConstraints = [(i, j, (/=)) | i <- [0..numClasses-1], j <- [0..numClasses-1], i /= j]
    let allConstraints = constraints ++ uniquenessConstraints
    let classDomains = [(i, [(d, t, r) | d <- [0..5], t <- [0..numTimeSlots-1], r <- [0..numRooms-1]]) | i <- [0..numClasses-1]]
    let filteredDomains = filterDomains classDomains startingValues
    let possibleSolutions = ac3 AC3 { cons = allConstraints, domains = filteredDomains }
    let solution = findSolution AC3 { cons = allConstraints, domains = possibleSolutions }
    case solution of
      Nothing -> putStrLn "No solution found."
      Just sol -> printSolution classNames dayNames roomNames timeSlotNames sol
\end{code}