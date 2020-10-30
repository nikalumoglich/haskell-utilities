import Data.List
import Data.Char

stringToList :: [Char] -> [Int]
stringToList d = map (digitToInt) (d)

productSum []     = 0
productSum (x:xs) = (x * ((length xs) + 2)) + (productSum xs)

verifyDigit digits | mod11 > 1 = 11 - mod11
                   | otherwise = 0
                   where mod11 = (productSum digits) `mod` 11

verifyDigits digits = show ((verifyDigit digits) * 10 + (verifyDigit (digits ++ [(verifyDigit digits)])))

main = do
    p <- getLine
    let params = stringToList p

    putStrLn (verifyDigits params)
