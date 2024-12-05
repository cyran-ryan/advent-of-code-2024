
import Data.List
main = do
    contents <- readFile "big_data"
    
    print (run contents)

run :: String -> Int
run contents =
    let list :: [(Int, Int)] = map (toTuple . map read . words) (lines contents)
        unzipped = unzip list
        left = sort (fst unzipped)
        right = sort (snd unzipped)
    in
        solveProblem (left, right, 0, 0)
        

toTuple :: [Int] -> (Int, Int)
toTuple [a, b] = (a, b)

solveProblem :: ([Int], [Int], Int, Int) -> Int
solveProblem (l:left, [], left_reps, right_reps) = l*left_reps*right_reps
solveProblem ([], right, _, _) = 0
solveProblem (l:left, r:right, right_reps, left_reps)
    | l == r = solveProblem (l:left, right, right_reps+1, left_reps)
    | null left || l == head left = solveProblem (left, r:right, right_reps, left_reps+1)
    | l < r = score + solveProblem (left, r:right, 0, 0)
    | l > r = score + solveProblem (l:left, right, 0, 0)
    where score = l*(left_reps + 1)*right_reps