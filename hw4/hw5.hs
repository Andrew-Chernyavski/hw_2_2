import Data.List
import System.IO


lists :: [Int] -> IO ()	
lists xs = do
              putStr ("choose command of \n 0 - exit \n 1 - insert \n 2 - delete \n 3 - print \n")
              command <- getLine
              case command of

			  "0" -> return ()

                          "1" -> do   
                                    putStr ("insert - ")
                                    value <- readLn
                                    lists (insert value xs)

                          "2" -> do
                                    putStrLn ("delete - ")
                                    value <- readLn
                                    lists (delete value xs)

                          "3" -> do
                                    putStr (show xs)
				    putStr ("\n")
                                    lists xs

                          otherwise -> lists xs
main = do
    lists []