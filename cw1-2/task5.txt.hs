isCorrect xs = (ret . filter (`elem` "(){}[]")) xs == Just ""
    where term sym (x:xs) | x == sym = Just xs
          term _ _ = Nothing

          ret ('(':xs) = ret xs >>= term ')' >>= ret
          ret ('{':xs) = ret xs >>= term '}' >>= ret
          ret ('[':xs) = ret xs >>= term ']' >>= ret
          ret xs = Just xs