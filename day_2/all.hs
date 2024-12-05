import Debug.Trace
import GHCi.Message (Message(InitLinker))
import System.IO (readFile')
data Direction = Up | Down | None | Unknown deriving (Enum, Show, Eq)

main = do
    contents <- readFile' "2024_02.txt"
    let reports :: [[Int]] = map (map read . words) (lines contents)
    putStrLn ("Part 1 " ++ show (sum (map (safetyScore Unknown) reports)))
    putStrLn ("Part 2 " ++ show (sum (map isSafe reports)))



sign :: Int -> Int
sign x
    | x < 0 = -1
    | x > 0 = 1
    | otherwise = 0

internalShouldTest :: [Int] -> Direction -> Int -> [Int]
internalShouldTest report dir i
    | i < 1 = internalShouldTest report dir 1
    | i == length report = []
    | left == right || abs (left - right) > 3 = [i-1, i, i+1]
    | dir == Unknown = internalShouldTest report currentDir (i+1)
    | dir /= currentDir = [0, i-1, i, i+1]
    | otherwise = internalShouldTest report dir (i+1)
    where left = report !! (i-1)
          right = report !! i
          currentDir = direction (left - right)

shouldTest :: [Int] -> [Int]
shouldTest xs = internalShouldTest xs Unknown 0


isSafeRemoval :: [Int] -> [Int] -> Int
isSafeRemoval [] x = 0
isSafeRemoval (i:xs) x
    | safetyScore Unknown (take i x ++ drop (i+1) x) == 1 = 1
    | otherwise = isSafeRemoval xs x

isSafe :: [Int] -> Int
isSafe x
    | safetyScore Unknown x == 1 = 1
    | isSafeRemoval (shouldTest x) x == 1 = 1
    | otherwise = 0

direction :: Int -> Direction
direction x
    | x < 0 = Down
    | x > 0 = Up
    | otherwise = None

safetyScore :: Direction -> [Int] -> Int
safetyScore _ []  = 1
safetyScore dir [x,y]
    | abs (x - y) > 3 || x == y = 0
    | dir /= Unknown && dir /= direction (x - y) = 0
    | otherwise = 1
    where result = x - y
          result_direction = direction result
safetyScore dir (first:second:xs)
    | abs result > 3 || result_direction == None = 0
    | dir == Unknown = safetyScore result_direction (second:xs)
    | dir /= result_direction = 0
    | dir == result_direction = safetyScore dir (second:xs)
    where result = first - second
          result_direction = direction result
