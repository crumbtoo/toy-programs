module Json where

import Text.Printf

data JS_Value = JS_Null
              | JS_String String
              | JS_Number Double
              | JS_Array [JS_Value]
              | JS_Object [(String, JS_Value)]
              | JS_Bool Bool
              deriving (Eq, Ord)

type JSON_Object = [(String, JS_Value)]

instance Show JS_Value where
    show JS_Null = "null"
    show (JS_String s) = show s
    show (JS_Number x) = show x
    show (JS_Array x) = show x
    show (JS_Object x) = printf "{%s}" $ showFields x
    show (JS_Bool x) = show x

showFields :: JSON_Object -> String
showFields [(k,v)] = printf "\"%s\":%s" k (show v) :: String
showFields ((k,v):xs) = printf "\"%s\":%s,%s" k (show v) (showFields xs) :: String

serialise :: JS_Value -> String
serialise = show
