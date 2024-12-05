import System.Environment
import System.IO (readFile')
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Map.Internal.Debug (showTree)
import Debug.Trace (trace)
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

middleNum :: [Int] -> Int
middleNum xs = xs !! (length xs `div` 2)

solve :: [[Int]] -> Map.Map Int [Int] -> Int
solve [] _ = 0
solve (x:xs) orderMap
    | processRow x orderMap = middleNum x + solve xs orderMap
    | otherwise = solve xs orderMap


toTuple :: [Int] -> (Int, Int)
toTuple [x, y] = (x, y)

getFileName :: [String] -> String
getFileName [] = "sample"
getFileName (x:_) = x