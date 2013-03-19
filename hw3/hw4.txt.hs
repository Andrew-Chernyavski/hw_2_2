data Tree a = Empty
            | Branch (Tree a) (Tree a)

length :: (Tree a) -> Int
length Empty = 0
length (Branch left right) = (max (length left) (length right)) + 1