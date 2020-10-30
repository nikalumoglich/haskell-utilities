import Data.Maybe

binSearch [] _ = Nothing
binSearch a  x
  | x == (a!!((length a) `quot` 2)) = Just ((length a) `quot` 2)
  | x > (a!!((length a) `quot` 2)) = (\y -> y + 1 + ((length a) `quot` 2)) <$> (binSearch (drop (((length a) `quot` 2) + 1) a) x)
  | otherwise = binSearch (take ((length a) `quot` 2) a) x

a = [0,1,2,3,4,5,6]


