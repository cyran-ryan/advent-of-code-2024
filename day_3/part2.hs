import Debug.Trace (trace)
main = do
    contents <- readFile "data"
    print (parse contents True Nothing Nothing)

readInt :: String -> Maybe Int
readInt s
    | not (null intVal) = Just (read intVal)
    | otherwise = Nothing
    where intVal = takeWhile (`elem` ['0'..'9']) s
removeInt :: String -> String
removeInt = dropWhile (`elem` ['0'..'9'])
parse :: String -> Bool -> Maybe Int -> Maybe Int -> Int
parse [] _ _ _ = 0
parse ('d':'o':'(':')':xs) False _ _ = parse xs True Nothing Nothing
parse ('d':'o':'n':'\'':'t':'(':')':xs) True _ _ = parse xs False Nothing Nothing
parse ('m':'u':'l':'(':xs) True Nothing Nothing
    | not (null leftInt) = parse (removeInt xs) True leftInt Nothing
    | otherwise          = parse xs True Nothing Nothing
    where leftInt = readInt xs
parse (',':xs) True (Just left) Nothing
    | not (null rightInt) = parse (removeInt xs) True (Just left) rightInt
    | otherwise           = parse xs True Nothing Nothing
    where rightInt = readInt xs

parse (')':xs) True (Just left) (Just right) = left * right + parse xs True Nothing Nothing

parse (x:xs) True _ _   =   parse (dropWhile (\x -> x /= 'm' && x /= 'd') xs) True Nothing Nothing
parse (x:xs) False _ _  =   parse (dropWhile (/='d') xs) False Nothing Nothing

