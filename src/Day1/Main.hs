module Main where
import System.IO
import Data.Maybe
import qualified Data.IntSet as S

main :: IO ()
main = do
    handle <- openFile "day1input" ReadMode
    input <- hGetContents handle
    let part1 = addNumbers (parseNumbers input)
    putStrLn (show part1)
    let part2 = firstDuplicate $ scanNumbers $ cycle (parseNumbers input)
    case part2 of
        Just x -> putStrLn (show x)
        Nothing -> putStrLn "No Duplicates Found"

parseNumbers :: String -> [Int]
parseNumbers input = map stringToInt $ lines $ filter (/= '+') input

stringToInt :: String -> Int
stringToInt = read

addNumbers :: [Int] -> Int
addNumbers = foldl (+) 0

scanNumbers :: [Int] -> [Int]
scanNumbers = scanl (+) 0

firstDuplicate :: [Int] -> Maybe Int
firstDuplicate xs = search S.empty xs
    where search _ [] = Nothing
          search ys (x:xs) = if S.member x ys then Just x else search (S.insert x ys) xs
