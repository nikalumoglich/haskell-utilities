import Data.List
import Data.Char
import System.Environment   

stringToList :: [Char] -> [Int]
stringToList d = map (digitToInt) (d)

digitList :: Int -> [Int]
digitList d = digitListAux d where
    digitListAux 0 = []
    digitListAux d = ((d `mod` 10) : (digitListAux (d `div` 10)))

getMultiplier xs = 2 - ((length xs) `mod` 2)

sumDigits 0 = 0
sumDigits x = x `mod` 10 + (sumDigits (x `div` 10))

sumVals [] = 0
sumVals (x:xs) = (sumDigits (x * (getMultiplier xs))) + (sumVals xs)

generateCreditCardWithLastDigit digits args =  (show digits) ++ (show missingDigit) ++ (show $ finalDigit args)
  where missingDigit = mod10 `div` 2 + (if mod10 `mod` 2 == 0 then 0 else 5)
        mod10 = 10 - (((sumVals (digitList digits)) + (finalDigit args)) `mod` 10)
        finalDigit [] = 4
        finalDigit (x:xs) = digitToInt x

main = do
    args <- getArgs                  -- IO [String]
    putStrLn (generateCreditCardWithLastDigit (read (args!!0)) (args!!1))
