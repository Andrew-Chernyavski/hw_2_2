helper :: Int -> Int -> [[Int]]
helper 0 _  = [[]]
helper n1 n2 = [1..n2] >>= (\x -> map (x:) (helper (n1 - x) (min x (n1 - x))))

decompose :: Int -> [[Int]]
decompose n = helper n n
          