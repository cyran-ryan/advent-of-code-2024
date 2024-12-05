import System.Environment
import System.IO (readFile')
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Map.Internal.Debug (showTree)
import Debug.Trace (trace)
import Data.List (sort, sortOn)
import Data.List.NonEmpty (sortWith)
import qualified Data.Ord
main = do

    args <- getArgs
    contents <- readFile' $ getFileName args
    let l = lines contents
    let ordering:: [(Int, Int)] = [toTuple $ map (read . T.unpack) (T.splitOn (T.pack "|") (T.pack x)) | x <- l, '|' `elem` x]
    let updates:: [[Int]] = [map (read . T.unpack) (T.splitOn (T.pack ",") (T.pack x))| x <- l, ',' `elem` x]
    let orderMap :: Map.Map Int [Int] = foldl (\acc x -> Map.insertWith (++) (fst x) [snd x] acc) Map.empty ordering
    print orderMap
    putStrLn $ showTree orderMap
    print $ solve updates orderMap

intersect :: [Int] -> [Int] -> [Int]
intersect [] = const []
intersect xs = filter (`elem` xs)

internalProcessRow :: [Int] -> Map.Map Int [Int] -> [Int] -> Bool
internalProcessRow [] _ _ = True
internalProcessRow (x:xs) orderMap foundNums =
    let nextNums = Map.findWithDefault [] x orderMap
        intersected = intersect nextNums foundNums
    in (null intersected && internalProcessRow xs orderMap (x:intersected))

processRow :: [Int] -> Map.Map Int [Int] -> Bool
processRow xs orderMap = internalProcessRow xs orderMap []

rowCycleInternal :: [Int] -> Map.Map Int [Int] -> [Int] -> [(Int, Int)]
rowCycleInternal [] _ _ = []
rowCycleInternal (x:xs) orderMap original =
    let nextNums = Map.findWithDefault [] x orderMap
        intersected = intersect nextNums original
    in
        if null intersected then
            (x, 0) : rowCycleInternal xs orderMap original
        else
            (x, length intersected) : rowCycleInternal xs orderMap original

rowCycle :: [Int] -> Map.Map Int [Int] -> [(Int, Int)]
rowCycle xs orderMap = rowCycleInternal xs orderMap xs


fixRow :: [Int] -> Map.Map Int [Int] -> [Int]
fixRow xs orderMap =
    map fst (sortOn (Data.Ord.Down . snd) cycle)
    where cycle = rowCycle xs orderMap

middleNum :: [Int] -> Int
middleNum xs = xs !! (length xs `div` 2)

solve :: [[Int]] -> Map.Map Int [Int] -> Int
solve [] _ = 0
solve (x:xs) orderMap
    | processRow x orderMap = solve xs orderMap
    | otherwise = middleNum fixedRow + solve xs orderMap
    where fixedRow = fixRow x orderMap

toTuple :: [Int] -> (Int, Int)
toTuple [x, y] = (x, y)

getFileName :: [String] -> String
getFileName [] = "sample"
getFileName (x:_) = x