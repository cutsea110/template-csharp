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

type Indent = Int

data Content = Raw String
             | Expr [E]
             | CForall String [E]
             | CMaybe String [E]
             | CNothing
             | CIf [E]
             | CElse
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

spaceTabs :: Parser String
spaceTabs = many (oneOf " \t")

doc = line `endBy` eol

line = (,) <$> indent <*> contents

contents = Raw <$> many (noneOf "\n\r")

indent :: Parser Int
indent = fmap sum $
         many ((char ' ' >> pure 1) <|>
               (char '\t' >> fail "Tabs are not allowed in indentation"))

{--
parser :: Parser [Content]
parser = many (try embed <|>
               try cforall <|>
               try cmaybe <|>
               try cnothing <|>
               try cif <|>
               try celse <|>
               raw)

embed :: Parser Content
embed = Expr <$> (string "#{" *> expr <* string "}")

expr :: Parser [E]
expr = many1 term
    where
      term :: Parser E
      term = spaceTabs *> (S <$> str <|> I <$> integer <|> V <$> var) <* spaceTabs

integer :: Parser Integer
integer = read <$> many1 digit

str :: Parser String
str = char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char
      quotedChar = noneOf "\\\"" <|> try (string "\\\"" >> return '"')

var :: Parser String
var = many1 (noneOf " \t\n\r{}#")

raw :: Parser Content
raw = Raw <$> many1 (noneOf "#\n\r")

cforall :: Parser Content
cforall = CForall <$> bindVal <*> expr <* eol
    where
      bindVal = string "#forall" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

cmaybe :: Parser Content
cmaybe = CMaybe <$> bindVal <*> expr <* eol
    where
      bindVal = string "#maybe" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

cnothing :: Parser Content
cnothing = string "#nothing" *> spaceTabs *> eol *> pure CNothing

cif :: Parser Content
cif = CIf <$> (string "#if" *> spaceTabs *> expr <* spaceTabs <* eol)

celse :: Parser Content
celse = string "#else" *> spaceTabs *> eol *> pure CElse

-- TODO: support pattern match
binding :: Parser String
binding = many1 (letter <|> digit <|> char '_')

-- | >>> parse parser "" "hello world#{foo var 12 \"ok\"} cutsea #{var} #forall x <- f 1 2 3  \t \nHello #if b 1 2 \n Hi,,  #else \n foo #maybe x <- mx 1 2 \n Bye#nothing  \n"
--}
