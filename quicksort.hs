import Data.List
import Data.Char

stringToList :: String -> [Int]
stringToList l = map (read) (words l)

quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
  where lesser = filter (< x) xs
        greater = filter (>= x) xs

main = do
  params <- getLine
  putStrLn $ show $ quicksort $ stringToList params