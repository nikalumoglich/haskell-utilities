import Control.Monad
import Data.List

toBin 0 = []
toBin x = (toBin (x `div` 2)) ++ (show (x `mod` 2))

main = do
  input <- getLine
  putStrLn $ toBin $ read input
