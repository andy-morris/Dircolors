module Attribute (
    Attributes(..), Attribute(..), Color(..),
    pattributes, pattribute,
  ) where

import P
import Text.Parsec
import Control.Applicative hiding ((<|>))

data Attributes = Inline [Attribute] | Ref String
  deriving (Show, Eq)

data Attribute = Bold | Underline | Blink | Reverse | Conceal
               | FG Color | BG Color
  deriving (Show, Eq)

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq, Show, Enum, Bounded)

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
