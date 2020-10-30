bubbleSort :: (Foldable t1, Ord t2) => t1 t2 -> [t2]
bubbleSort = foldr swapTill []

swapTill x [] = [x]
swapTill x (y:xs) = min x y : swapTill (max x y) xs


sp x [] = [x]
sp x (y:xs) = y : sp x xs
