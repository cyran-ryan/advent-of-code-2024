import System.Environment
import Distribution.Compat.Prelude (exitWith, catMaybes)
import System.Exit (die)
import Control.Monad
data Direction = N | S | E | W deriving (Show)
data XMASPuzzle = XMASPuzzle {
    rowLength :: Int,
    searchString :: String
} deriving (Show)
main = do
    args <- getArgs
    contents <- readFile $ getFileName args
    let xmasSearch = lines contents
    unless (verifyStringLength xmasSearch) $ die "Invalid input"
    let l = length (head xmasSearch)
    let searchString = [x | x <- contents, x /= '\n']
    let xs = findAsRow searchString 0
    let puzzle = XMASPuzzle l searchString
    print $ validateAllCrosses puzzle xs


moveIndex :: Direction -> Int -> XMASPuzzle -> Int
moveIndex N index puzzle = index - rowLength puzzle
moveIndex S index puzzle = index + rowLength puzzle
moveIndex E index puzzle = index + 1
moveIndex W index puzzle = index - 1

move :: [Direction] -> XMASPuzzle -> Maybe Int -> Maybe Int
move [] _ (Just index) = Just index
move (N:dirs) puzzle (Just index)
    | newIndex < 0 = Nothing
    | otherwise = move dirs puzzle (Just newIndex)
    where newIndex = moveIndex N index puzzle
move (S:dirs) puzzle (Just index)
    | newIndex >= length (searchString puzzle) = Nothing
    | otherwise = move dirs puzzle (Just newIndex)
    where newIndex = moveIndex S index puzzle
move (E:dirs) puzzle (Just index)
    | newIndex `mod` rowLength puzzle == 0 = Nothing
    | otherwise = move dirs puzzle (Just newIndex)
    where newIndex = moveIndex E index puzzle
move (W:dirs) puzzle (Just index)
    | index `mod` rl == 0 = Nothing
    | otherwise = move dirs puzzle (Just newIndex)
    where newIndex = moveIndex W index puzzle
          rl = rowLength puzzle
move _ _ Nothing = Nothing


getCrossWords :: XMASPuzzle -> Int -> String
getCrossWords puzzle index = map (searchString puzzle !!) $ catMaybes [ne, sw, nw, se]
    where ne = move [N,E] puzzle (Just index)
          sw = move [S,W] puzzle (Just index)
          nw = move [N,W] puzzle (Just index)
          se = move [S,E] puzzle (Just index)

verifyStringLength :: [String] -> Bool
verifyStringLength [] = True
verifyStringLength [x] = True
verifyStringLength (x:y:xs) = length x == length y && verifyStringLength (y:xs)

findAsRow :: String -> Int -> [Int]
findAsRow [] _ = []
findAsRow ('A':xs) index = index : findAsRow xs (index + 1)
findAsRow (_:xs) index = findAsRow xs (index + 1)

verifyCrossWords :: String -> Bool
verifyCrossWords [] = True
verifyCrossWords [x] = False
verifyCrossWords ('M':'S':xs) = verifyCrossWords xs
verifyCrossWords ('S':'M':xs) = verifyCrossWords xs
verifyCrossWords x = False

isValidCross :: XMASPuzzle -> Int -> Bool
isValidCross puzzle index
    | length crossword == 4 && verifyCrossWords crossword = True
    | otherwise = False
    where crossword = getCrossWords puzzle index

validateAllCrosses :: XMASPuzzle -> [Int] -> Int
validateAllCrosses _ [] = 0
validateAllCrosses puzzle (x:xs) 
    | x `mod` rl == 0 = validateAllCrosses puzzle xs
    | x `mod` rl == rl = validateAllCrosses puzzle xs
    | x < rl = validateAllCrosses puzzle xs
    | x >= length (searchString puzzle) - rl = validateAllCrosses puzzle xs
    | isValidCross puzzle x = 1 + validateAllCrosses puzzle xs
    | otherwise = validateAllCrosses puzzle xs
    where rl = rowLength puzzle

getFileName :: [String] -> String
getFileName [] = "sample"
getFileName (x:_) = x
