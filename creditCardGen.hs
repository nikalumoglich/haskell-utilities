import Data.Char ( digitToInt )
import System.Environment ( getArgs )   

stringToList :: [Char] -> [Int]
stringToList d = map (digitToInt) (d)

digitList :: Int -> [Int]
digitList d = digitListAux d where
    digitListAux 0 = []
    digitListAux d = ((d `mod` 10) : (digitListAux (d `div` 10)))

getMultiplier :: Foldable t => t a -> Int
getMultiplier xs = 2 - ((length xs) `mod` 2)

sumDigits :: Integral p => p -> p
sumDigits 0 = 0
sumDigits x = x `mod` 10 + (sumDigits (x `div` 10))

sumVals :: [Int] -> Int
sumVals [] = 0
sumVals (x:xs) = (sumDigits (x * (getMultiplier xs))) + (sumVals xs)

generateCreditCardWithLastDigit :: Int -> [Char] -> [Char]
generateCreditCardWithLastDigit digits args =  (show digits) ++ (show missingDigit) ++ (show $ finalDigit args)
  where missingDigit = mod10 `div` 2 + (if mod10 `mod` 2 == 0 then 0 else 5)
        mod10 = 10 - (((sumVals (digitList digits)) + (finalDigit args)) `mod` 10)
        finalDigit [] = 4
        finalDigit (x:_) = digitToInt x

main :: IO ()
main = do
    args <- getArgs                  -- IO [String]
    putStrLn (generateCreditCardWithLastDigit (read (args!!0)) (args!!1))
