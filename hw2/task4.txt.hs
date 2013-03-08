findValuePos :: Eq a => [a]->a->Integer
findValuePos list value = findRec list value 1
findRec :: Eq a => [a]->a->Integer->Integer
findRec [] _ _ = error "The list doesn't contain this element"
findRec (x:xs) y ind = if (x == y)
                       then ind
                       else findRec xs y (ind + 1)