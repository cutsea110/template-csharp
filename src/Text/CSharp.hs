{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.CSharp where

import Control.Applicative ((<$>), (<*>))
import Text.ParserCombinators.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Quote

csharp :: QuasiQuoter
csharp = QuasiQuoter { quoteExp = csharpFromString }

-- | C# code gen
csharpFromString :: String -> Q Exp
csharpFromString = litE . stringL


data Content = Raw String
             | Expr [String]
               deriving Show

data E = S String
       | I Integer
       | V String
       deriving Show

parser :: Parser [Content]
parser = many (try embed <|> raw)

embed :: Parser Content
embed = Expr <$> (string "#{" *> expr <* string "}")
    where
      expr :: Parser [String]
      expr = many1 term
      term = spaces *> many1 (noneOf " \t}")

integer :: Parser Integer
integer = read <$> many1 digit

str :: Parser String
str = char '"' *> many quotedChar <* char '"'
quotedChar :: Parser Char
quotedChar = noneOf "\\\"" <|> try (string "\\\"" >> return '"')

raw :: Parser Content
raw = Raw <$> many1 (noneOf "#")

