import Data.List
import Data.Char

stringToList :: [Char] -> [Int]
stringToList d = map (digitToInt) (d)

getMultiplier xs = 2 - ((length xs) `mod` 2)

sumDigits 0 = 0
sumDigits x = x `mod` 10 + (sumDigits (x `div` 10))

sumVals [] = 0
sumVals (x:xs) = (sumDigits (x * (getMultiplier xs))) + (sumVals xs)

getVerifyDigit input = sumVals $ stringToList input 

main = do
    param <- getLine
    putStrLn $ show (getVerifyDigit param)