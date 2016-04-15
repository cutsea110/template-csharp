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
             | Expr String
               deriving Show

parser :: Parser [Content]
parser = many (try embed <|> raw)

embed :: Parser Content
embed = Expr <$> (string "#{" *> expr <* string "}")
    where
      expr :: Parser String
      expr = many1 (noneOf "}")

raw :: Parser Content
raw = Raw <$> many1 (noneOf "#")
