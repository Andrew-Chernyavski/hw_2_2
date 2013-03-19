data Tree a = Empty
            | Branch (Tree a) (Tree a)

lengthMin :: (Tree a) -> Int
lengthMin Empty = 0
lengthMin (Branch left right) = (min (lengthMin left) (lengthMin right)) + 1