import Data.List

stringToList :: String -> [Int]
stringToList l = map (read) (words l)

filterNext :: (Ord t, Num t) => [t] -> t -> [t] -> [[t]]
filterNext stack capacity [] = []
filterNext stack capacity (y:ys) = [ filter (\x -> x <= capacity - y) (stack ++ ys) ] ++ (filterNext (stack ++ [y]) capacity ys)

validCombinations' :: (Num t, Ord t) => t -> [[t]] -> [t] -> [[t]]
validCombinations' _        []     _ = []
validCombinations' capacity (f:fs) (t:ts) = (if (length subComb > 0) then (map (\x -> t:x) subComb) else (if t <= capacity then [[t]] else [[]])) ++ (validCombinations' capacity fs ts)
  where
    subComb = (validCombinations' (capacity-t) (filterNext [] (capacity-t) f) f )
    
validCombinations :: (Num t, Ord t) => t -> [t] -> [[t]]
validCombinations capacity items = validCombinations' capacity (filterNext [] capacity items) items

maxAllocation :: Int -> [Int] -> [Int]
maxAllocation capacity sizes = foldr1 (\x y -> if foldr1 (+) x >= foldr1 (+) y then x else y) possiblePermutations where 
  possiblePermutations = validCombinations capacity sizes
