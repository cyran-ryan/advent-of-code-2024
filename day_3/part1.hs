import Debug.Trace (trace)
main = do
    contents <- readFile "data"
    print (parse contents Nothing Nothing)

readInt :: String -> Maybe Int
readInt s
    | not (null intVal) = Just (read intVal)
    | otherwise = Nothing
    where intVal = takeWhile (`elem` ['0'..'9']) s
removeInt :: String -> String
removeInt = dropWhile (`elem` ['0'..'9'])
parse :: String -> Maybe Int -> Maybe Int -> Int
parse [] _ _ = 0
parse ('m':'u':'l':'(':xs) Nothing Nothing
    | not (null leftInt) = parse (removeInt xs) leftInt Nothing
    | otherwise = parse xs Nothing Nothing
    where leftInt = readInt xs
parse (',':xs) (Just left) Nothing
    | not (null rightInt) = parse (removeInt xs) (Just left) rightInt
    | otherwise = parse xs (Just left) Nothing
    where rightInt = readInt xs
parse (')':xs) (Just left) (Just right) = left * right + parse xs Nothing Nothing
parse (x:xs) _ _ = parse (dropWhile (/='m') xs) Nothing Nothing

