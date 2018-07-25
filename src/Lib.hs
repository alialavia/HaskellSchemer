module Lib
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "sampleFilePath" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

