module Json.Pretty where

#define INDENT_WIDTH 4

unroll :: String -> String
unroll "" = ""
unroll (s:st)
    | s == '{' || s == '[' =
        s : '\n' : unroll st
    | s == '}' || s == ']' =
        '\n' : s : unroll st
    | s == ',' =
        ",\n" ++ unroll st
    | s == ':' = -- append a newline if followed by an opening brace
        ':' : (let nxt = head st in
            if nxt == '{' || nxt == '[' then "\n" else "") ++ unroll st
    | s == ' ' =
        unroll st
    | otherwise = 
        s : unroll st

showDepth :: Int -> String
showDepth 0 = ""
showDepth d = tab ++ showDepth (d - 1)
    where tab = replicate INDENT_WIDTH ' '

indent' :: [String] -> Int -> [String]
indent' [] _ = []
indent' (x@(s:st):xs) depth
    | s == '{' || s == '[' =
            (showDepth depth ++ x) : indent' xs (depth + 1)
    | s == '}' || s == ']' =
            (showDepth (depth - 1) ++ x) : indent' xs (depth - 1)
    | otherwise =
        (showDepth depth ++ x) : indent' xs depth

indent :: String -> String
indent s = take (length iota - 1) iota
    where iota = unlines $ indent' (lines s) 0
-- `take (length iota - 1)` removes the last element from the list,
-- which is an unwanted newline that `unlines` adds

space :: String -> String
space "" = ""
space [s] = [s]
space (s:w@(c:st)) = if s == ':' && c /= '\n'
               then ": " ++ space w
               else s : space w

beautify :: String -> String
beautify s = (indent.unroll) s
