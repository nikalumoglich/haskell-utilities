import Control.Monad
import Data.List

stringToList :: String -> [Int]
stringToList l = map (read) (words l)

doFactor :: [[Int]] -> [Int]
--doFactor lst = map sum lst
doFactor lst = orderByZeros

main = do
    p <- getLine
    lines <- replicateM (read p) getLine
    let input = map stringToList lines
    putStrLn "----"
    mapM putStrLn (map show (doFactor input))

    --print $ length lines


    rightMostPylon xs = rightMostPylonIndex $ reverse xs where
      rightMostPylonIndex (y:ys) | y == 1 = length ys
                                | otherwise = rightMostPylonIndex ys
      rightMostPylonIndex [] = (-1)
    
    pylons i k xs
                   | length xs <= i = 0
                   | i < 0 = (-1)
                   | index < 0 = (-1)
                   | otherwise = if next == -1 then (-1) else 1 + next
      where index = rightMostPylon (take (k+i) xs)
            next = (pylons (k-1) k (drop (index+1) xs))