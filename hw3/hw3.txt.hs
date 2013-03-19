import Data.List (elemIndex)
import Data.Maybe (fromJust)

findMaxSumOfNeighb :: (Integral a) => [a] -> Int
findMaxSumOfNeighb x = fromJust (elemIndex (maximum list) list) + 1
		   where list = zipWith (+) x (tail x)