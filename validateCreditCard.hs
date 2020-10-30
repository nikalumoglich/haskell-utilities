import Data.List
import System.Environment   
import Data.Char

stringToList :: [Char] -> [Int]
stringToList d = map (digitToInt) (d)

getMultiplier xs = 2 - ((length xs) `mod` 2)

sumDigits 0 = 0
sumDigits x = x `mod` 10 + (sumDigits (x `div` 10))

sumVals [] = 0
sumVals (x:xs) = (sumDigits (x * (getMultiplier (x:xs)))) + (sumVals xs)

verifyCreditCard input = (sumVals $ stringToList input) `mod` 10 == 0

main = do
    args <- getArgs                  -- IO [String]
    putStrLn $ show (verifyCreditCard (args!!0))