module Main where
import System.IO
import Data.List

type Range = (Int,Int) -- Low, High

doesOverlap :: (Range,Range) -> Bool
doesOverlap ((low1,high1),(low2,high2)) = not $ null $ intersect range1 range2
    where range1 = [low1..high1]
          range2 = [low2..high2]

doesContain :: (Range,Range) -> Bool
doesContain ((low1,high1),(low2,high2)) = range1 `isInfixOf` range2
    where range1 = [low1..high1]
          range2 = [low2..high2]

parseRange :: String -> Range 
parseRange s = (read low, read $ drop 1 high)
    where (low,high) = span (/= '-') s

parseLine :: String -> (Range,Range)
parseLine line = (parseRange first, parseRange $ drop 1 second)
    where (first,second) = span (/= ',') line

part1 :: String -> Int 
part1 = length . filter (== True) . map (doesContain . parseLine) . lines

part2 :: String -> Int 
part2 = length . filter (== True) . map (doesOverlap . parseLine) . lines

main :: IO ()
main = do 
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ "Part1: " ++ show (part1 contents)
    print $ "Part2: " ++ show (part2 contents)