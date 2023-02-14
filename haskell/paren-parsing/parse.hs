-- breakBrackets :: String -> [String]
-- breakBrackets (s:st) = if s == '['
--                        then breakBrackets st
--                        else

-- innerBrackets :: String -> String
-- innerBrackets (s:st) = skipAfter

lose :: Int -> [a] -> [a]
lose _ [] = []
lose n x = take (length x - n) x

f :: String -> String
f = (g.h)

g :: String -> String
g "" = ""
g (s:st) = if s == '('
           then st
           else g st

h :: String -> String
h "" = ""
h s = if l == ')'
      then sh
      else h sh
      where l = last s
            sh = lose 1 s

innermostParen :: String -> String
innermostParen s = if '(' `elem` s
                   then innermostParen ((g.h) s)
                   else s

main = do
    let s = "what (the (dogeing) fauci)?"
    -- print (breakBrackets s)
    -- print (innerBrackets s)
    print s
    print (f s)
    print (innermostParen s)
