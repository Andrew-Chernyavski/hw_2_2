evenNum1 :: [Int] -> Int
evenNum1 xs = sum (map (\y -> 1 - ( mod y 2)) xs)

evenNum2 :: [Int] -> Int
evenNum2 xs =  length (filter (even) xs)

evenNum3 :: [Int] -> Int
evenNum3 xs = foldr (\ x y -> y + 1 - (mod x 2)) 0 xs