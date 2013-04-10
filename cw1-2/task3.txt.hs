import GHC.IO
sp = ' ':sp
st = '*':st
printSp x = take x sp
printSt x = take x st

printLn x = uPrint x (x - 1)
  		where uPrint a b
	  		| b > 0 = be:cen ++ [t]   
	  		| b == 0 = [printSt (2 * a - 1)]
	  		| otherwise = []
					where   be = printSp b ++ printSt (2 * a - 1 - 2*b) ++ printSp b		    
						cen = uPrint a (b-1)
						t = printSp b ++ printSt (2 * a - 1 - 2*b) ++ printSp b
draw x =  mapM print (printLn x)