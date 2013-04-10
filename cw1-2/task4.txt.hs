supermap :: [(a -> b)] -> [a] -> [b]
supermap f [] = []
supermap [] x = []
supermap (f:fs) x = (map f x) ++ supermap fs x