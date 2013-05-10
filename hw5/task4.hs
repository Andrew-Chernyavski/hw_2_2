generate :: Int -> [Int]
generate n = [1..n] >>= \x -> [1..n] >>= (\y -> [x*y])