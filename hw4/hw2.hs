data Tree a = Nil
            | Branch a (Tree a) (Tree a)

fold :: (a -> a -> a) -> a -> (Tree a) -> a	
fold _ z Nil = z
fold func z (Branch v left right) = fold func (func v (fold func z left)) right 