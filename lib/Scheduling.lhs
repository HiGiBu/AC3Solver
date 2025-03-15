\begin{code}
module Scheduling where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (replicateM)
import Data.List (elemIndex)
import AC3Solver

type ClassAssignment = (Int, Int, Int)

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

getConstraint :: [String] -> IO (Maybe [ConstraintAA Int ClassAssignment])
getConstraint classNames = do
  putStrLn "Enter a constraint (e.g., 'class1 is before class2' or 'class1 is the same day as class2'). Type 'Done' to finish:"
  input <- getLine
  if input == "Done" then return Nothing else do
    let parts = words input
    case parts of
      [class1, "is", "before", class2] -> Just <$> processClasses class1 class2 "is before" classNames
      [class1, "is", "the", "same", "day", "as", class2] -> Just <$> processClasses class1 class2 "is the same day as" classNames
      _ -> do
        putStrLn "Invalid input format."
        getConstraint classNames

processClasses :: String -> String -> String -> [String] -> IO [ConstraintAA Int ClassAssignment]
processClasses class1 class2 keyword classNames = do
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

schedulingMain :: IO ()
schedulingMain = do
  (numClasses, numRooms, numTimeSlots, classNames, roomNames, timeSlotNames) <- getUserInputs
  constraints <- collectConstraints classNames
  
  let uniquenessConstraints = [(i, j, (/=)) | i <- [0..numClasses-1], j <- [0..numClasses-1], i /= j]
  let allConstraints = constraints ++ uniquenessConstraints
  let classDomains = [(i, [(d, r, t) | d <- [0..5], t <- [0..numTimeSlots-1], r <- [0..numRooms-1]]) | i <- [0..numClasses-1]]
  let ac3Inst = AC3 { cons = allConstraints, domains = classDomains }

  let solutions = ac3 ac3Inst

  putStrLn "\nPossible Solutions:"
  print solutions

\end{code}