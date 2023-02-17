module Json.Parse where

import Json

import Data.Char
import Data.Bits

deserialise :: String -> Maybe JS_Value
deserialise s = parseElement s

-- JSON whitespace characters are <space>, <newline>,
-- <carriage-return>, and <horizontal-tab>
isWhitespace :: Char -> Bool
isWhitespace c = (c == '\x0020' ||
                  c == '\x000a' ||
                  c == '\x000d' ||
                  c == '\x0009')

trim :: String -> String
trim = (trimStart.trimEnd)
    where trimStart whole@(s:st) = if isWhitespace s
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
    -- | filter isWhitespace s

parseArray :: String -> Maybe JS_Value
parseArray s = Nothing

-- characters := ""
--             | character characters
-- character := ['0020'-'10ffff'] - '\\' - ' '
--            | '\' escape
parseCharacters :: String -> String
parseCharacters "" = ""

-- codepoint escape ('u' hex hex hex hex)
parseCharacters ('\\':'u':w:x:y:z:st)
    | allAreHex = (chr $ ((digitToInt w) * 16^3) +
                         ((digitToInt x) * 16^2) +
                         ((digitToInt y) * 16^1) +
                         ((digitToInt z) * 16^0))
                   : parseCharacters st
    | otherwise = 'u' : w : x : y : z : parseCharacters st

    where allAreHex = and $ map (flip elem $ hexabet) [w,x,y,z]
          hexabet = ['A'..'F'] ++ ['a'..'f'] ++ ['0'..'9']
                  
-- escape := '"'
--         | '\'
--         | '/'
--         | 'b'
--         | 'f'
--         | 'n'
--         | 'r'
--         | 't'
--         | 'u' hex hex hex hex
parseCharacters ('\\':s:st)
    | s == '"'  = '"'  : parseCharacters st
    | s == '\\' = '\\' : parseCharacters st
    | s == '/'  = '/'  : parseCharacters st
    | s == 'b'  = '\b' : parseCharacters st
    | s == 'f'  = '\f' : parseCharacters st
    | s == 'n'  = '\n' : parseCharacters st
    | s == 'r'  = '\r' : parseCharacters st
    | s == 't'  = '\t' : parseCharacters st
    | otherwise = s : parseCharacters st

parseCharacters (s:st) =
    if (ord s `inRange` (0x20, 0x10ffff)) && (s /= '"' && s /= '\\')
    then s : parseCharacters st
    else ""

    where c `inRange` (a,b) = c >= a && c <= b

-- string := '"' characters '"'
parseString :: String -> Maybe JS_Value
parseString (s:st) = if s == '"' && last st == '"'
                     then Just $ JS_String $ parseCharacters inside
                     else Nothing

                     -- `st` is already past the leading quote
                     where inside = take (length st - 1) st

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
