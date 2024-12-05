import System.IO
import Control.Monad
import Data.Bifunctor
import Data.List
main = do
    contents <- readFile "big_data"
    let l :: ([String], [String]) = unzip (map (toTuple . words) (lines contents))
    print (sum (map abs ( zipWith (-) (sort (map read (fst l))) (sort (map read (snd l))))))

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)
