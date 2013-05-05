import Data.List (group)

noReps :: (Eq a) => [a] -> Bool
noReps x = (length x == length (group x))