import Control.Monad
import Data.List

{-

 class  Monad m  where
  (>>=)            :: m a -> (a -> m b) -> m b
  (>>)             :: m a -> m b -> m b
  return           :: a -> m a
  fail             :: String -> m a

-}



toBin 0 = []
toBin x = (toBin (x `div` 2)) ++ (show (x `mod` 2))



--main = getLine >>= (\x -> putStrLn x >> getLine >>= (\y -> putStrLn ("y: " ++ y)))

main = do 
  x <- getLine
  putStrLn x
  y <- getLine
  putStrLn ("y: " ++ y)

  
  {- do
  input <- getLine
  putStrLn $ toBin $ read input
  -}