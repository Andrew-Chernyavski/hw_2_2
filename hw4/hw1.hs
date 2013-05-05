data Tree a = Nil
            | Branch a (Tree a) (Tree a)

findByCond :: (a -> Bool) -> (Tree a) -> Bool
findByCond cond Nil = False
findByCond cond (Branch v left right) = if cond v 
					then True
					else ((findByCond cond left) || (findByCond cond right))