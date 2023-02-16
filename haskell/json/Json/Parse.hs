module Json.Parse where

import Json

import Data.Char

deserialise :: String -> Maybe JS_Value
deserialise s = parseElement s

trim :: String -> String
trim = (trimStart.trimEnd)
    where trimStart whole@(s:st) = if s == ' ' || s == '\t' || s == '\n' || s == '\r'
                                   then trimStart st
                                   else whole
          trimEnd s = (reverse.trimStart.reverse) s

-- element := whitespace value whitespace
parseElement :: String -> Maybe JS_Value
parseElement s = (parseValue.trim) s

-- object := '{' whitespace '}'
--         | '{' members '}'
parseObject :: String -> Maybe JS_Value
parseObject s = Nothing

parseArray :: String -> Maybe JS_Value
parseArray s = Nothing

-- takeLeadQuote :: String -> Maybe String
-- takeLeadQuote ('"':st) = Just st
-- takeLeadQuote _ = Nothing

-- parseCharacter :: String -> Maybe Char
-- parseCharacter (s:st)
--     | ord s >= 0x20 && ord s <= 0x10FFFF = Just s

-- parseCharacters :: String -> Maybe String
-- parseCharacters (s:st) = parseCharacter s : parseCharacters st

parseString :: String -> Maybe JS_Value
parseString s = Just $ JS_String $ "strrring"

-- number := integer fraction exponent
parseNumber :: String -> Maybe JS_Value
parseNumber s = Nothing

parseValue :: String -> Maybe JS_Value
parseValue w@(s:st)
    | s == '{'                  = parseObject w
    | s == '['                  = parseArray w
    | s == '"'                  = parseString w
    | parseNumber w /= Nothing  = parseNumber w
    | w == "true"               = Just (JS_Bool True)
    | w == "false"              = Just (JS_Bool False)
    | w == "null"               = Just JS_Null
    | otherwise                 = Nothing
