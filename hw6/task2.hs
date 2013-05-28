simplPol :: [Int] -> [Int]
simplPol pol = if null pol then [] 
			   else if (last pol) /= 0 then pol 
			   else simplPol(init pol)

printComp :: Int -> Int -> String
printComp 0 _ = ""
printComp a 0 = (if (a > 0) then "+"
				 else "-") ++ show (abs a)
printComp a 1 = (if (a == 1) then "+"
				 else if (a == -1) then "-"
				 else printComp (abs a) 0) ++ "x"
printComp a i = (if (a == 1) then "+"
				 else if (a == -1) then "-"
				 else printComp (abs a) 0) ++ "x^" ++ show i

printEld :: [Int] -> String
printEld pol = if null pol then ""
			   else printComp (last pol) ((length pol) - 1)

printPol :: [Int] -> String
printPol pol = printEld (simplPol pol) ++ if null pol then ""
													  else printPol (init (if null (simplPol pol) then [0]
																								  else (simplPol pol)))
													  
													  
degOfItem :: [Int] -> Int -> Int
degOfItem p i = if i < (length p) then p!!i else 0																								  
																								  
sumUp :: [Int] -> [Int] -> [Int]
sumUp p1 p2 = simplPol [(degOfItem p1 i) + (degOfItem p2 i) | i <- [0..(max (length p1) (length p2)) - 1]]