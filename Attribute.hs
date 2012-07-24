module Attribute (
    Attributes(..), Attribute(..), Color(..),
    pattributes, pattribute,
  ) where

import P
import Data.Char
import Text.Parsec
import Control.Applicative hiding ((<|>))
import Text.PrettyPrint.Free hiding (equals, comma)

data Attributes = Inline [Attribute] | Ref String
  deriving (Show, Eq)

data Attribute = Bold | Underline | Blink | Reverse | Conceal
               | FG Color | BG Color
  deriving (Show, Eq)

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq, Show, Enum, Bounded)

instance Pretty Attributes where
    pretty (Inline attrs) = hsep . punctuate "," $ map pretty attrs
    pretty (Ref sty)      = text sty

instance Pretty Attribute where
    pretty (FG x) = hsep ["fg", "=", pretty x]
    pretty (BG x) = hsep ["bg", "=", pretty x]
    pretty x      = text . map toLower $ show x

instance Pretty Color where
    pretty = text . map toLower . show

pattributes :: P Attributes
pattributes =
      (Ref    <$> identifier "style"          <?> "style name")
  <|> (Inline <$> (pattribute `sepBy1` comma) <?> "attribute list")
  <|> (word "reset" (Inline [])               <?> "\"reset\"")

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
pcolorA str c = do reserved str; equals; c <$> pcolor
  <?> "color attribute"

pcolor :: P Color
pcolor = choice (map word' [minBound .. maxBound])
  <?> "color name"
