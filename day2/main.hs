module Main where
import System.IO
import Data.Char
import Data.Maybe

data Gesture = Rock | Paper | Scissors

data Outcome = Win | Loss | Draw

type Score = Int

chooseGesture :: (Gesture,Outcome) -> Gesture
chooseGesture (opponent,goal) = case opponent of 
    Rock -> case goal of 
            Win -> Paper
            Loss -> Scissors 
            Draw -> Rock
    Paper -> case goal of 
            Win -> Scissors
            Loss -> Rock
            Draw -> Paper
    Scissors -> case goal of 
            Win -> Rock
            Loss -> Paper
            Draw -> Scissors

getOutcome :: (Gesture,Gesture) -> Outcome
getOutcome (opponent,player) = case opponent of 
    Rock -> case player of 
            Rock -> Draw 
            Paper -> Win 
            Scissors -> Loss 
    Paper -> case player of 
            Rock -> Loss 
            Paper -> Draw 
            Scissors -> Win 
    Scissors -> case player of 
            Rock -> Win 
            Paper -> Loss 
            Scissors -> Draw

outcomeToScore :: Outcome -> Score 
outcomeToScore outcome = case outcome of 
    Win -> 6
    Loss -> 0
    Draw -> 3

computeScorePart1 :: (Gesture,Gesture) -> Score 
computeScorePart1 (opponent,player) = gestureScore + outcomeScore
    where gestureScore = gestureToScore player
          outcomeScore = outcomeToScore $ getOutcome (opponent,player)


computeScorePart2 :: (Gesture,Outcome) -> Score 
computeScorePart2 (opponent,player) = gestureScore + outcomeScore
    where choice = chooseGesture (opponent,player)
          gestureScore = gestureToScore choice
          outcomeScore = outcomeToScore $ getOutcome (opponent,choice)


parseLine :: String -> (Gesture,Gesture)
parseLine l =  (head gestures,last gestures)
    where gestures = map (letterToGesture) $ filter (not . isSpace) l

parseLine2 :: String -> (Gesture,Outcome)
parseLine2 l =  (letterToGesture $ head trimmed, letterToOutcome $ last trimmed)
    where trimmed = filter (not . isSpace) l

letterToGesture :: Char -> Gesture
letterToGesture c 
    | c == 'A' =  Rock
    | c == 'B' =  Paper
    | c == 'C' =  Scissors
    | c == 'X' =  Rock
    | c == 'Y' =  Paper
    | c == 'Z' =  Scissors

letterToOutcome :: Char -> Outcome
letterToOutcome c  
    | c == 'X' = Loss 
    | c == 'Y' = Draw 
    | c == 'Z' = Win

gestureToScore :: Gesture -> Score 
gestureToScore g = 
    case g of 
        Rock -> 1 
        Paper -> 2
        Scissors-> 3

computeTotalScorePart1 :: [String] -> Score 
computeTotalScorePart1 = sum . map (computeScorePart1 . parseLine)

computeTotalScorePart2 :: [String] -> Score 
computeTotalScorePart2 = sum . map (computeScorePart2 . parseLine2)

main :: IO ()
main = do 
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ "Part1: " ++ (show $ computeTotalScorePart1 $ lines $ contents)
    print $ "Part2: " ++ (show $ computeTotalScorePart2 $ lines $ contents)
