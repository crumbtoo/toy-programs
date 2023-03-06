module Json where

import Data.Char hiding (isControl)
import Data.List
import Text.Printf

data JS_Value = JS_Null
            | JS_Bool Bool
            | JS_String String
            | JS_Number { int :: Integer, frac :: [Int], exponent :: Integer }
            | JS_Array [JS_Value]
            | JS_Object [(String, JS_Value)]
            deriving (Eq)

instance Show JS_Value where
  show value = case value of
    JS_Null          -> "null"
    JS_Bool True     -> "true"
    JS_Bool False    -> "false"
    JS_String s      -> showJSONString s
    JS_Number s [] 0 -> show s
    JS_Number s f 0  -> show s ++ "." ++ concatMap show f
    JS_Number s [] e -> show s ++ "e" ++ show e
    JS_Number s f e  -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
    JS_Array a       -> "[" ++ intercalate ", " (map show a) ++ "]"
    JS_Object o      -> "{" ++ intercalate ", " (map showKV o) ++ "}"
    where
      showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
    '\'' -> "'"
    '\"' -> "\\\""
    '\\' -> "\\\\"
    '/'  -> "\\/"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
    _ -> [c]
    where
        showJSONNonASCIIChar c =
            let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a
        showHex = printf "%x"
