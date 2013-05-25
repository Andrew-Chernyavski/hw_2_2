check s "" = null s
check s (c:cs) = if (c `elem` "(<{[")
    then check (c:s) cs
    else if (null s || not (((head s):c:[]) `elem` ["()", "<>", "{}", "[]"]))
            then False
            else check (tail s) cs

checkLine :: String -> Bool
checkLine = check "" . filter (`elem` "()<>{}[]")