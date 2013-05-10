checkFor :: (Eq a) => (a -> Bool) -> [a] -> Bool
checkFor b l = all b l