import Data.List

data BT a = Nil
			| Node a (BT a) (BT a)
			deriving (Eq, Show)

numOfElems :: BT a -> Int
numOfElems Nil = 0
numOfElems (Node _ left right) = 1 + (numOfElems left) + (numOfElems right)

height :: BT a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

findElem :: (Ord a) => BT a -> a -> Bool
findElem Nil _ = False
findElem (Node y left right) x | x < y = findElem left y
                             | x > y = findElem right y
                             | otherwise = True

addElem :: (Ord a) => BT a -> a -> BT a
addElem Nil x = Node x Nil Nil
addElem node@(Node y left right) x  | x < y = Node y (addElem left x) right
								| x > y = Node y left (addElem right x)
								| otherwise = node
								
minElem (Node n left right) = if left == Nil
                                                      then n
                                                      else minElem left
delElem :: (Ord a) => BT a -> a -> BT a
delElem Nil _ = Nil
delElem (Node y left right) x | x < y = Node y (delElem left x) right
                             | x > y = Node y left (delElem right x)
                             | otherwise = delete
							 where delete | right == Nil = left
										  | otherwise = Node (minElem right) left (delElem right (minElem right))