import Data.List
import Data.Char

stringToList :: [Char] -> [Int]
stringToList d = map (digitToInt) (d)

productSum []     = 0
productSum (x:xs) = (x * ((length xs) + 2)) + (productSum xs)

verifyDigit digits | mod11 > 1 = 11 - mod11
                   | otherwise = 0
                   where mod11 = (productSum digits) `mod` 11

verifyDigits digits = (show firstVerifyDigit) ++ (show secondVerifyDigit)
  where numbers = stringToList digits
        firstVerifyDigit = verifyDigit numbers
        secondVerifyDigit = verifyDigit (numbers ++ [firstVerifyDigit])

verify digits = (verifyDigits (take 9 digits)) == (drop 9 digits)

main = do
    params <- getLine
    putStrLn $ show (verify params)