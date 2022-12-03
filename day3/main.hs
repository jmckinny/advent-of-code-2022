import Data.Char
import System.IO
import Data.List

type Priority = Int

getIntersect :: (String,String) -> String
getIntersect (first,second) = nub $ intersect first second

splitLine :: String -> (String,String)
splitLine line = (take (len `div` 2) line, drop (len `div` 2) line)
        where len = length line

getPriority :: Char -> Priority
getPriority c 
    | isLower c =  (+1) $ (`mod` 97) $ ord $ c
    | isUpper c = (+27) $ (`mod` 65) $ ord $ c

getLinePriority :: String -> Priority
getLinePriority = sum . map (getPriority)

getGroupBadge :: [String] -> String 
getGroupBadge lst = nub $ intersect third $ intersect first second
    where first = lst !! 0
          second = lst !! 1
          third = lst !! 2

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n lst = first : (splitEvery n rest)
    where (first,rest) = splitAt n lst



part1 :: String -> Priority
part1 = sum . map (getLinePriority . getIntersect . splitLine) . lines

part2 :: String -> Priority
part2 = sum . map (getLinePriority . getGroupBadge) . splitEvery 3 . lines

main :: IO ()
main = do 
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ "Part1: " ++ (show $ part1 $ contents)
    print $ "Part2: " ++ (show $ part2 $ contents)