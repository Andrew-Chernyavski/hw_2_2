-- a ()
-- b {}
-- c []
-- d <>

checkLine :: String -> Bool
checkLine (x:xs) = parse 0 0 0 0 (x:xs)
		where 
		parse a b c d [] = ((a == 0) && (b == 0) && (c == 0) && (d == 0))
		parse a b c d (y:ys) | ((a < 0) || (b < 0) || (c < 0) || (d < 0)) = False
				      | otherwise = if y == '(' then parse (a + 1) b c d ys
					       else if y == ')' then parse (a - 1) b c d ys
					       else if y == '{' then parse a (b + 1) c d ys
					       else if y == '}' then parse a (b - 1) c d ys
					       else if y == '[' then parse a b (c + 1) d ys
					       else if y == ']' then parse a b (c - 1) d ys
					       else if y == '<' then parse a b c (d + 1) ys
					       else if y == '>' then parse a b c (d - 1) ys
                                               else parse a b c d ys
					      
