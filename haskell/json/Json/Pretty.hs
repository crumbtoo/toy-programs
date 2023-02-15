module Json.Pretty where

unroll :: String -> String
unroll "" = ""
unroll (s:st) =
    case s of
        '{' ->
            "{\n" ++ unroll st

        '}' ->
            "\n}" ++ unroll st

        '[' ->
            "[\n" ++ unroll st

        ']' ->
            "\n]" ++ unroll st

        ',' ->
            ",\n" ++ unroll st

        ':' -> -- append a newline if followed by an opening brace
            ':' : (let nxt = head st in
                if nxt == '{' || nxt == '[' then "\n" else "") ++ unroll st

        ' ' ->
            unroll st

        _ ->
            s : unroll st

showDepth :: Int -> String
showDepth 0 = ""
showDepth d = "  " ++ showDepth (d - 1)

indent' :: [String] -> Int -> [String]
indent' [] _ = []
indent' (x@(s:st):xs) depth =
    case s of
        '{' ->
            (showDepth depth ++ x) : indent' xs (depth + 1)

        '}' ->
            (showDepth (depth - 1) ++ x) : indent' xs (depth - 1)

        '[' ->
            (showDepth depth ++ x) : indent' xs (depth + 1)
        ']' ->
            (showDepth (depth - 1) ++ x) : indent' xs (depth - 1)

        _ ->
            (showDepth depth ++ x) : indent' xs depth

indent :: String -> String
indent s = unlines $ indent' (lines s) 0

beautify :: String -> String
beautify s = (indent.unroll) s
