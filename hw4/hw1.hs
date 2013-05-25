import Control.Monad
import Data.List

data BTree a = Null
                  | Node a (BTree a) (BTree a)
    deriving (Show)

findByCond :: (a -> Bool) -> BTree a -> Maybe a
findByCond _ Null = Nothing
findByCond p (Node x l r) | p x = Just x
                          | otherwise = findByCond p l `mplus` findByCond p r