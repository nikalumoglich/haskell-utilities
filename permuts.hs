permuts (x:[]) = [[x]]
permuts (x:xs) = foldr1 (++) (map (combs [] x) (permuts xs)) where
  combs p a []     = [ p ++ [a] ]
  combs p a (y:ys) = [ p ++ [a] ++ (y:ys) ] ++ (combs (p ++ [y]) a ys)
