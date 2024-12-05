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
    let xs = findXsRow searchString 0
    let puzzle = XMASPuzzle l searchString
    print $ getXMAS (resolveStrings puzzle xs)


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

getValidIndexesInDirection :: XMASPuzzle -> [Direction] -> Int -> [Int]
getValidIndexesInDirection puzzle [] index = [index]
getValidIndexesInDirection puzzle dirs index =
    let moveInDir = move dirs puzzle
    in catMaybes (take 3 (drop 1 $ iterate moveInDir (Just index)))

verifyStringLength :: [String] -> Bool
verifyStringLength [] = True
verifyStringLength [x] = True
verifyStringLength (x:y:xs) = length x == length y && verifyStringLength (y:xs)

findXsRow :: String -> Int -> [Int]
findXsRow [] _ = []
findXsRow ('X':xs) index = index : findXsRow xs (index + 1)
findXsRow (_:xs) index = findXsRow xs (index + 1)


assertXsRow :: String -> [Int] -> Bool
assertXsRow _ [] = True
assertXsRow str (y:ys) = str !! y == 'X' && assertXsRow str ys

resolveStringInDirection :: XMASPuzzle -> [Direction] -> Int -> Maybe String
resolveStringInDirection puzzle dir index =
    case getValidIndexesInDirection puzzle dir index of
        [x, y, z] -> Just [searchString puzzle !! index, searchString puzzle !! x, searchString puzzle !! y, searchString puzzle !! z]
        _ -> Nothing

resolveStringsFromIndex :: XMASPuzzle -> Int -> [String]
resolveStringsFromIndex puzzle index =
    catMaybes [
        resolveStringInDirection puzzle [N] index,
        resolveStringInDirection puzzle [S] index,
        resolveStringInDirection puzzle [E] index,
        resolveStringInDirection puzzle [W] index,
        resolveStringInDirection puzzle [N,E] index,
        resolveStringInDirection puzzle [N,W] index,
        resolveStringInDirection puzzle [S,E] index,
        resolveStringInDirection puzzle [S,W] index
    ]

resolveStrings :: XMASPuzzle -> [Int] -> [String]
resolveStrings _ [] = []
resolveStrings puzzle (x:xs) = resolveStringsFromIndex puzzle x ++ resolveStrings puzzle xs

isXMAS :: String -> Bool
isXMAS [] = False
isXMAS ['X', 'M', 'A', 'S'] = True

getXMAS :: [String] -> Int
getXMAS [] = 0
getXMAS (['X', 'M', 'A', 'S']:xs) = 1 + getXMAS xs
getXMAS (_:xs) = getXMAS xs

getFileName :: [String] -> String
getFileName [] = "sample"
getFileName (x:_) = x
