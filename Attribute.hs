module Attribute where

import P
import Data.Char
import Text.Parsec
import Control.Applicative hiding ((<|>))
import Text.PrettyPrint.Free

data Attribute = Bold | Underline | Blink | Reverse | Conceal
               | FG Color | BG Color
  deriving (Show, Eq)

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq, Show, Enum, Bounded)

instance Pretty Color where
    pretty = text . map toLower . show

instance Pretty Attribute where
    pretty (FG x) = hsep ["fg", "=", pretty x]
    pretty (BG x) = hsep ["bg", "=", pretty x]
    pretty x      = text . map toLower $ show x

pattributes :: P [Attribute]
pattributes = pattribute `sepBy1` symbol "," <?> "attribute list"

pattribute :: P Attribute
pattribute = choice
    [word' Bold,
     word' Underline,
     word' Blink,
     word' Reverse,
     word' Conceal,
     pcolorA "fg" FG,
     pcolorA "bg" BG]
  <?> "attribute"

pcolorA :: String -> (Color -> Attribute) -> P Attribute
pcolorA str c = do
    word str ()
    symbol "="
    c <$> pcolor
  <?> "color attribute"

pcolor :: P Color
pcolor = choice (map word' [minBound .. maxBound])
  <?> "color name"