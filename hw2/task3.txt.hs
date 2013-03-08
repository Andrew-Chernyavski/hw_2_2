sumOfDig :: Integer -> Integer
sumOfDig 0 = 0
sumOfDig n = ((mod n 10) + (sumOfDig (div n 10)))