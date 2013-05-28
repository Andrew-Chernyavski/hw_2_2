import Data.Maybe

data BTree  = EmptyTree
           | Node Char BTree BTree
              deriving (Show,Eq)
			  
encode :: BTree -> String
encode EmptyTree  = "e"
encode (Node x left right) = 'n':x:(encode left ++ encode right)

decode' :: String -> Maybe (BTree, String)
decode' ('e':cs) = return (EmptyTree, cs)
decode' ('n':c:cs) = decode' cs >>= (\x -> decode' (snd x) 
                                >>= (\y -> return ( Node c (fst x) (fst y), snd y)))
decode' _  = Nothing
decode :: String -> Maybe BTree
decode str = decode' str >>= \x -> if (snd x /= "") then Nothing
													else return (fst x)