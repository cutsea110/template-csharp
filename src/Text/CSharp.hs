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
             | CForall String [E]
             | CMaybe String [E]
             | CNothing
               deriving Show

data E = S String
       | I Integer
       | V String
       deriving Show

eol :: Parser String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> fail "end of line"

parser :: Parser [Content]
parser = many (try embed <|>
               try cforall <|>
               try cmaybe <|>
               raw)

embed :: Parser Content
embed = Expr <$> (string "#{" *> expr <* string "}")

expr :: Parser [E]
expr = many1 term
    where
      term :: Parser E
      term = spaces *> (S <$> str <|> I <$> integer <|> V <$> var)

integer :: Parser Integer
integer = read <$> many1 digit

str :: Parser String
str = char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char
      quotedChar = noneOf "\\\"" <|> try (string "\\\"" >> return '"')

var :: Parser String
var = many1 (noneOf " \t}")

raw :: Parser Content
raw = Raw <$> many1 (noneOf "#")

cforall :: Parser Content
cforall = CForall <$> bindVal <*> expr
    where
      bindVal = string "#forall" *> spaces *>
                binding
                <* spaces <* string "<-" <* spaces

cmaybe :: Parser Content
cmaybe = CMaybe <$> bindVal <*> expr
    where
      bindVal = string "#maybe" *> spaces *>
                binding
                <* spaces <* string "<-" <* spaces

cnothing :: Parser Content
cnothing = string "#nothing" *> many (noneOf "\r\n") *> eol >> return CNothing

-- TODO: support pattern match
binding :: Parser String
binding = many1 (letter <|> digit <|> char '_')

-- | >>> parse parser "" "hello world#{foo var 12 \"ok\"} cutsea #{var}"
