import Data.List

stringToList :: String -> [Int]
stringToList l = map (read) (words l)

maxAllocation :: Int -> [Int] -> [Int]
maxAllocation capacity sizes = foldr1 (\x y -> if foldr1 (+) x >= foldr1 (+) y then x else y) possiblePermutations where 
  possiblePermutations = map (tryCapacities capacity []) (permutations sizes)

tryCapacities _ stack [] = stack
tryCapacities capacity stack sizes
  | subject == capacity = stack ++ [subject]
  | subject <= capacity = tryCapacities (capacity - subject) (stack ++ [subject]) (filter (\x -> x <= (capacity - subject)) (tail sizes)) 
  | otherwise = tryCapacities capacity stack (filter (\x -> x <= capacity) (tail sizes)) 
  where subject = head sizes

main = do
  c <- getLine
  s <- getLine
  let capacity = read c
  let sizes = stringToList s
  putStrLn (show (maxAllocation capacity sizes))