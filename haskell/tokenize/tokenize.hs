module Tokenize (tokenize) where

trim :: String -> String
trim s = (trimStart.trimEnd) s
    where trimStart whole@(s:st) = if s == ' '
                                   then trimStart st
                                   else whole
          trimEnd s = (reverse.trimStart.reverse) s

firstWord :: String -> String
firstWord s = (firstWord'.trim) s
    where firstWord' [c] = [c]
          firstWord' (s:st) = if s == ' '
                              then ""
                              else [s] ++ (firstWord' st)

notFirstWord :: String -> String
notFirstWord = (notFirstWord'.trim)
    where notFirstWord' "" = ""
          notFirstWord' (s:st) = if s == ' '
                                 then trim st
                                 else notFirstWord' st

tokenize :: String -> [String]
tokenize "" = []
tokenize s = [firstWord s] ++ tokenize (notFirstWord s)


main = do
    s <- getLine
    print(tokenize s)
