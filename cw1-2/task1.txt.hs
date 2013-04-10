genEd = 1: map ((*) (-1)) genEd
mult (x:xs) (y:ys) = x*y : mult xs ys
generate = mult genEd [1, 2 ..]