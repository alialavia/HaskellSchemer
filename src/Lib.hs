module Lib
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
-- space ∷ Stream s m Char ⇒ ParsecT s u m Char
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

