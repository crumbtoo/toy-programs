module Json.Parse where

import Json
import Data.Char (digitToInt, isDigit, isHexDigit, chr, ord)
import Data.Bits
import Data.Functor (($>))
import Control.Applicative
import Control.Monad
import Numeric

-- the Parser type constructor takes a single argument of
-- type `i -> Maybe (i, o)`
newtype Parser i o = Parser { runParser :: i -> Maybe (i, o) }

instance Functor (Parser i) where
    fmap f parser = Parser $ fmap (fmap f) . runParser parser

instance Applicative (Parser i) where
    pure x    = Parser $ pure . (, x)
    pf <*> po = Parser $ \input ->
        case runParser pf input of
            Nothing        -> Nothing
            Just (rest, f) -> fmap f <$> runParser po rest

instance Monad (Parser i) where
    p >>= f = Parser $ \input -> case runParser p input of
        Nothing        -> Nothing
        Just (rest, o) -> runParser (f o) rest

instance Alternative (Parser i) where
    empty = Parser $ const empty
    p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound  = 0xDC00
lowSurrogateUpperBound  = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a = ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a  = ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a     = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b = chr $
  ((ord a - highSurrogateLowerBound) `shiftL` 10)
  + (ord b - lowSurrogateLowerBound) + 0x10000

satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \xs ->
    case xs of
        (x:xs) -> if predicate x then Just (xs, x) else Nothing
        _ -> Nothing

char1 :: Char -> Parser String Char
char1 c = Parser $ \xs ->
    case xs of
        (x:xs) -> if x == c then Just (xs, x) else Nothing
        _ -> Nothing

char :: Char -> Parser String Char
char c = satisfy (==c)

digit1 :: Parser String Int
digit1 = Parser $ \i ->
    case runParser (satisfy isDigit) i of
        Nothing      -> Nothing
        Just (i', o) -> Just (i', digitToInt o)

digit2 :: Parser String Int
digit2 = Parser $ \i -> case runParser (satisfy isDigit) i of
    Nothing      -> Nothing
    Just (i', o) -> Just $ fmap digitToInt $ (i', o)

-- the outer-most `fmap` applies to the tuple, the
-- inner-most `fmap` applies to the Maybe.
--
-- fmap (+2) (Just 2) == Just 4
-- fmap (+2) (0,2) == (0,4)
-- fmap (fmap (+2)) (Just (0,2)) == Just (0,4)
digit3 :: Parser String Int
digit3 = Parser $ \i -> fmap (fmap digitToInt) $ runParser (satisfy isDigit) $ i

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

string1 :: String -> Parser String String
string1 ""     = Parser $ \i -> Just (i, "")
string1 (c:cs) = Parser $ \i ->
    case runParser (char c) i of
        Nothing        -> Nothing
        Just (rest, _) ->
            case runParser (string1 cs) rest of
                Nothing -> Nothing
                Just (rest', _) -> Just (rest', c:cs)

string2 :: String -> Parser String String
string2 s = case s of
    ""     -> Parser $ pure . (, "")
    (c:cs) -> Parser $ \i ->
        case runParser (char c) i of
            Nothing        -> Nothing
            Just (rest, c) -> fmap (c:) <$> runParser (string2 cs) rest

string :: String -> Parser String String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

jNull :: Parser String JS_Value
jNull = string "null" $> JS_Null

jBool :: Parser String JS_Value
jBool = string "true" $> JS_Bool True <|> string "false" $> JS_Bool False

jsonEscape1 :: Parser String Char
jsonEscape1 = Parser $ \(c:cs) ->
    case c of
        '"'   ->  Just      (cs,  '"')
        '\\'  ->  Just      (cs,  '\\')
        '/'   ->  Just      (cs,  '/')
        'b'   ->  Just      (cs,  '\b')
        'f'   ->  Just      (cs,  '\f')
        'n'   ->  Just      (cs,  '\n')
        't'   ->  Just      (cs,  '\t')
        'u'   ->  runParser (fourHex) cs
        _     ->  Nothing
    where
        fourHex :: Parser String Char
        fourHex = Parser $ \s ->
            case s of
                -- i seriously went for the greek alphabet
                -- before choosing to use more than one
                -- character in a variable name. oh my god.
                w@(α:β:γ:δ:ω) | all isHexDigit w ->
                    Just (ω, chr.fst.head.readHex $ w)
                _ ->
                    Nothing

jsonChar1 :: Parser String Char
jsonChar1 = Parser $ \(c:cs) ->
    case c of
        '"' ->
            Nothing
        '\\' ->
            runParser jsonEscape1 cs
        c | c `inInterval` ('\x20','\x10ffff') ->
            Just (cs,c)
        _ ->
            Nothing
    where x `inInterval` (a,b) = (x >= a) && (x <= b)

jsonEscape :: Parser String Char
jsonEscape =   char '"' $> '"'
           <|> char '/' $> '/'
           <|> char 'b' $> '\b'
           <|> char 'f' $> '\f'
           <|> char 'n' $> '\n'
           <|> char 'r' $> '\r'
           <|> char 't' $> '\t'
           <|> Parser (\('u':w@(_:_:_:_:ω)) ->
               if all isHexDigit w then
                   Just (ω, chr.fst.head.readHex $ w)
               else Nothing)
           <|> Parser (const Nothing)

-- jsonChar :: String -> Maybe (String, Char)
jsonChar :: Parser String Char
jsonChar =   char '\\' *> jsonEscape
         <|> satisfy (\c -> c `inInterval` ('\x20','\x10ffff') && c /= '"' && c /= '\\')
    where x `inInterval` (a,b) = (x >= a) && (x <= b)

jsString :: Parser String JS_Value
jsString = JS_String <$> (char '"' *> jsString')
    where
        jsString' = do
            optFirst <- optional jsonChar
            case optFirst of
                Nothing ->
                    "" <$ char '"'
                Just first | not (isSurrogate first) ->
                    (first:) <$> jsString'
                Just first -> do
                    second <- jsonChar
                    if isHighSurrogate first && isLowSurrogate second then
                        (combineSurrogates first second :) <$> jsString'
                    else
                        empty

