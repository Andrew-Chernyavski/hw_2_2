findExtr' x [] = x 
findExtr' (Just x) (a:xs)
    | a > x && xs == [] = Nothing
    | a > x && a > (xs !! 0) = Just a
    | otherwise = findExtr' (Just a) xs

findExtr :: [Int] -> Maybe Int
findExtr [] = Nothing
findExtr [x] = Nothing
findExtr (x:xs) | x > xs!! 0 = Nothing
				| otherwise  = findExtr' (Just x) xs