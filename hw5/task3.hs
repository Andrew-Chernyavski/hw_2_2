generate 0 = []
generate 1 = [[1],[2],[3]]
generate n = [1, 2, 3] >>= (\x -> map (x:) (generate (n - 1)))