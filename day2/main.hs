module Main where
import System.IO
import Data.Char
import Data.Maybe

data Gesture = Rock | Paper | Scissors

data Outcome = Win | Loss | Draw

type Score = Int



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

parseLine :: String -> (Gesture,Gesture)
parseLine l =  (head gestures,last gestures)
    where gestures = map (letterToGesture) $ filter (not . isSpace) l


letterToGesture :: Char -> Gesture
letterToGesture c 
    | c == 'A' =  Rock
    | c == 'B' =  Paper
    | c == 'C' =  Scissors
    | c == 'X' =  Rock
    | c == 'Y' =  Paper
    | c == 'Z' =  Scissors

gestureToScore :: Gesture -> Score 
gestureToScore g = 
    case g of 
        Rock -> 1 
        Paper -> 2
        Scissors-> 3

computeTotalScorePart1 :: [String] -> Score 
computeTotalScorePart1 = sum . map (computeScorePart1 . parseLine)

main :: IO ()
main = do 
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ "Part1: " ++ (show $ computeTotalScorePart1 $ lines $ contents)