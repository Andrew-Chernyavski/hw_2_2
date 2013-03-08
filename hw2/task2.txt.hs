generatePow2 :: Integer -> [Integer]
generatePow2 n = [2^x | x <- [1..n]]