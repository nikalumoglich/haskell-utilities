import Data.List
import Data.Char
import System.Random

digitList :: Int -> [Int]
digitList d = reverse $ digitListAux d where
    digitListAux 0 = []
    digitListAux d = ((d `mod` 10) : (digitListAux (d `div` 10)))

toInt :: Float -> Int
toInt x = round x

format2d x | length x > 1 = x
           | otherwise = format2d ("0" ++ x)  

productSum []     = 0
productSum (x:xs) = (x * ((length xs) + 2)) + (productSum xs)

verifyDigit digits | mod11 > 1 = 11 - mod11
                   | otherwise = 0
                   where mod11 = (productSum digits) `mod` 11

verifyDigits digits = format2d $ show ((verifyDigit digits) * 10 + (verifyDigit (digits ++ [(verifyDigit digits)])))

main = do
    rand <- randomIO :: IO Float
    let num = toInt(rand * 899999999) + 100000000
    putStrLn ((show num) ++ (verifyDigits $ digitList num))
