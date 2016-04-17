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
             | Quoted [Expr]
             | CtrlForall String [Expr]
             | CtrlMaybe String [Expr]
             | CtrlNothing
             | CtrlIf [Expr]
             | CtrlElse
             | CtrlCase [Expr]
             | CtrlOf [Expr]
             | CtrlLet String [Expr]
               deriving Show

data Expr = S String
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

doc :: Parser [(Indent, [Content])]
doc = line `endBy` eol

line :: Parser (Indent, [Content])
line = (,) <$> indent <*> contents

indent :: Parser Indent
indent = fmap sum $
         many ((char ' ' >> pure 1) <|>
               (char '\t' >> fail "Tabs are not allowed in indentation"))

contents :: Parser [Content]
contents = many (try quoted <|>
                 try ctrlForall <|>
                 try ctrlMaybe <|>
                 try ctrlNothing <|>
                 try ctrlIf <|>
                 try ctrlElse <|>
                 try ctrlCase <|>
                 try ctrlOf <|>
                 try ctrlLet <|>
                 raw)

quoted :: Parser Content
quoted = Quoted <$> (string "${" *> expr <* string "}")

ctrlForall :: Parser Content
ctrlForall = CtrlForall <$> bindVal <*> expr
    where
      bindVal = string "$forall" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

ctrlMaybe :: Parser Content
ctrlMaybe = CtrlMaybe <$> bindVal<*> expr
    where
      bindVal = string "$maybe" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

ctrlNothing :: Parser Content
ctrlNothing = string "$nothing" *> spaceTabs >> pure CtrlNothing

ctrlIf :: Parser Content
ctrlIf = CtrlIf <$> (string "$if" *> spaceTabs *> expr <* spaceTabs)

ctrlElse :: Parser Content
ctrlElse = string "$else" *> spaceTabs >> pure CtrlElse

ctrlCase :: Parser Content
ctrlCase = CtrlCase <$> (string "$case" *> spaceTabs *> expr <* spaceTabs)

ctrlOf :: Parser Content
ctrlOf = CtrlOf <$> (string "$of" *> spaceTabs *> expr <* spaceTabs)

ctrlLet :: Parser Content
ctrlLet = CtrlLet <$> bindVal <*> expr
    where
      bindVal = string "$let" *> spaceTabs *>
                binding
                <* spaceTabs <* string "=" <* spaceTabs

-- TODO: support pattern match
binding :: Parser String
binding = many1 (letter <|> digit <|> char '_')

expr :: Parser [Expr]
expr = spaceTabs *> many1 term
    where
      term :: Parser Expr
      term = (S <$> str <|> I <$> integer <|> V <$> var) <* spaceTabs

integer :: Parser Integer
integer = read <$> many1 digit

str :: Parser String
str = char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char
      quotedChar = noneOf "\\\"" <|> try (string "\\\"" >> return '"')

var :: Parser String
var = many1 (noneOf " \t\n\r{}$")

raw :: Parser Content
raw = Raw <$> many1 (noneOf "$\n\r")

