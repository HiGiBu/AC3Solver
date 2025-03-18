\subsection{The Scheduling library}\label{sec:Scheduling}

\begin{code}
module Scheduling where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (replicateM)
import Data.List (elemIndex)
import AC3Solver
import Backtracking (findSolution)

type ClassAssignment = (Int, Int, Int)
dayNames :: [String]
dayNames = ["monday", "tuesday", "wednesday", "thursday", "friday"]

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
  replicateM n getLine

getUserInputs :: IO (Int, Int, Int, [String], [String], [String])
getUserInputs = do
  numClasses <- parseInput "Enter the number of classes:"
  classNames <- getNames "Enter class names:" numClasses
  numRooms <- parseInput "Enter the number of rooms:"
  roomNames <- getNames "Enter room names:" numRooms
  numTimeSlots <- parseInput "Enter the number of time slots per day:"
  timeSlotNames <- getNames "Enter time slot names:" numTimeSlots
  return (numClasses, numRooms, numTimeSlots, classNames, roomNames, timeSlotNames)

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

filterDomains :: [Domain Int ClassAssignment] -> [(Agent Int, ClassAssignment -> Bool)] -> [Domain Int ClassAssignment]
filterDomains domainList conditions =
    [(agent, [v | v <- values, all (\(a, f) -> (a /= agent) || f v) conditions]) | (agent, values) <- domainList]

getConstraint :: [String] -> IO (Maybe [ConstraintAA Int ClassAssignment])
getConstraint classNames = do
  putStrLn "Enter a constraint (e.g., 'class1 is before class2' or 'class1 is the same day as class2'). Type 'Done' to finish:"
  input <- getLine
  if input == "Done" then return Nothing else do
    let parts = words input
    case parts of
      [class1, "is", "before", class2] -> Just <$> processConstraints class1 class2 "is before" classNames
      [class1, "is", "the", "same", "day", "as", class2] -> Just <$> processConstraints class1 class2 "is the same day as" classNames
      _ -> do
        putStrLn "Invalid input format"
        getConstraint classNames

processConstraints :: String -> String -> String -> [String] -> IO [ConstraintAA Int ClassAssignment]
processConstraints class1 class2 keyword classNames = do
  case (elemIndex class1 classNames, elemIndex class2 classNames) of
    (Just i, Just j) -> case keyword of
      "is before" -> return [(i, j, checkBefore), (j, i, checkAfter)]
      "is the same day as"  -> return [(i, j, checkSameDay), (j, i, checkSameDay)]
      _           -> error "Invalid keyword"
    _ -> error "Invalid class names"

collectConstraints :: [String] -> IO [ConstraintAA Int ClassAssignment]
collectConstraints classNames = do
  let loop acc = do
        constraint <- getConstraint classNames
        case constraint of
          Nothing -> return acc
          Just cs  -> loop (cs ++ acc)
  loop []

getStartingValues :: [String] -> [String] -> [String] -> IO (Maybe (Agent Int, ClassAssignment -> Bool))
getStartingValues classNames roomNames timeslotNames = do
  putStrLn "Enter known values (e.g., 'class1 is in room3', 'class1 is at 11am' or 'class1 is on monday'). Type 'Done' to finish:"
  input <- getLine
  if input == "Done" then return Nothing else do
    let parts = words input
    case parts of
      [class1, "is", "in", room] -> Just <$> processStartingValues class1 room "is in" classNames roomNames
      [class1, "is", "at", time] -> Just <$> processStartingValues class1 time "is at" classNames timeslotNames
      [class1, "is", "on", day] -> Just <$> processStartingValues class1 day "is on" classNames dayNames
      _ -> do
        putStrLn "Invalid input format"
        getStartingValues classNames roomNames timeslotNames

processStartingValues :: String -> String -> String -> [String] -> [String] -> IO (Agent Int, ClassAssignment -> Bool)
processStartingValues class1 value keyword classNames valueNames = do
  case (elemIndex class1 classNames, elemIndex value valueNames) of
    (Just i, Just j) -> case keyword of
      "is in" -> return (i, checkRoom j)
      "is at" -> return (i, checkTime j)
      "is on" -> return (i, checkDay j)
      _           -> error "Invalid keyword"
    _ -> error "Invalid name"

collectStartingValues :: [String] -> [String] -> [String] -> IO [(Agent Int, ClassAssignment -> Bool)]
collectStartingValues classNames roomNames timeslotNames = do
  let loop acc = do
        value <- getStartingValues classNames roomNames timeslotNames
        case value of
          Nothing -> return acc
          Just svs  -> loop (svs : acc)
  loop []

printSolution :: [String] -> [String] -> [String] -> [String] -> [(Agent Int, ClassAssignment)] -> IO ()
printSolution classNames days roomNames timeSlotNames list = putStrLn $ concat
  [classNames !! agent ++ " is scheduled on " ++ days !! dayId ++
    " in " ++ roomNames !! roomId ++ " at " ++ timeSlotNames !! timeId ++ ".\n"
  | (agent, (dayId, roomId, timeId)) <- list]

schedulingMain :: IO ()
schedulingMain = do
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

\end{code}