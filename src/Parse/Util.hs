module Parse.Util where

import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String
import           Text.Parsec.Prim

keyword :: String -> Parser String
keyword k = try (string k <* notFollowedBy alphaNum)

operator :: String -> Parser String
operator s = pad (keyword s)

-- Surround helpers

surroundChar :: Char -> Char -> Parser a -> Parser a
surroundChar o c = between (char o) (char c)

pad :: Parser a -> Parser a
pad = between spaces spaces

parens :: Parser a -> Parser a
parens = surroundChar '(' ')'

braces :: Parser a -> Parser a
braces = surroundChar '{' '}'

brackets :: Parser a -> Parser a
brackets = surroundChar '[' ']'

singleQuotes :: Parser a -> Parser a
singleQuotes = surroundChar '\'' '\''

doubleQuotes :: Parser a -> Parser a
doubleQuotes = surroundChar '"' '"'

-- Operator helpers

increment :: Parser String
increment = keyword "++"

decrement :: Parser String
decrement = keyword "--"

-- Character helpers

-- 34
doubleQuote :: Parser Char
doubleQuote = char '"'

-- 39
singleQuote :: Parser Char
singleQuote = char '\''

-- 42
star :: Parser Char
star = char '*'

-- 44
comma :: Parser Char
comma = char ','

-- 59
semicolon :: Parser Char
semicolon = char ';'

-- 92
backslash :: Parser Char
backslash = char '\\'

-- 95
underscore :: Parser Char
underscore = char '_'

-- 96
backtick :: Parser Char
backtick = char '`'

-- Combinator helpers

(<:>) a b = (:) <$> a <*> b

(<++>) a b = (++) <$> a <*> b

(<+++>) a b = a <++> toList b

toPair :: a -> b -> (a, b)
toPair a b = (a, b)

toList :: Functor f => f a -> f [a]
toList = fmap (: [])
