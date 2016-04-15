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
             | Expr [E]
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
      expr :: Parser [E]
      expr = many1 term
      term :: Parser E
      term = spaces *> (S <$> str <|> I <$> integer <|> V <$> var)

integer :: Parser Integer
integer = read <$> many1 digit

str :: Parser String
str = char '"' *> many quotedChar <* char '"'
quotedChar :: Parser Char
quotedChar = noneOf "\\\"" <|> try (string "\\\"" >> return '"')

var :: Parser String
var = many1 (noneOf " \t}")

raw :: Parser Content
raw = Raw <$> many1 (noneOf "#")
