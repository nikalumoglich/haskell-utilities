import Data.List
import Data.Char

stringToList :: [Char] -> [Int]
stringToList d = map (digitToInt) (d)

getMultiplier xs = ((length xs) `mod` 2) + 1

sumDigits 0 = 0
sumDigits x = x `mod` 10 + (sumDigits (x `div` 10))

sumVals [] = 0
sumVals (x:xs) = (sumDigits (x * (getMultiplier (x:xs)))) + (sumVals xs)

generateVerifyDigit input = (10 - ((sumVals $ stringToList input) `mod` 10)) `mod` 10

main = do
    param <- getLine
    putStrLn $ show (generateVerifyDigit param)