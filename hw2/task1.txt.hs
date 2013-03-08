revers :: [a] -> [a]
revers list = moveTo list []
moveTo :: [a] -> [a] -> [a]
moveTo [] list = list
moveTo (x:xs) list = moveTo xs (x:list)