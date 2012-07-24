module Attribute (
    Attribute(..), Color(..),
    pattributes, pattribute,
    toConf,
  ) where

import P
import Data.Char
import Data.List
import Text.Parsec
import Control.Applicative hiding ((<|>))
import Text.PrettyPrint.Free hiding (equals, comma)

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
pattributes =
      (pattribute `sepBy1` comma <?> "attribute list")
  <|> (word "reset" [] <?> "\"reset\"")

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

toConf :: [Attribute] -> String
toConf [] = "00"
toConf xs = intercalate ";" $ map toConf' xs where
  toConf' Bold      = "01"
  toConf' Underline = "04"
  toConf' Blink     = "05"
  toConf' Reverse   = "07"
  toConf' Conceal   = "08"
  toConf' (FG x)    = '3' : colToConf x
  toConf' (BG x)    = '4' : colToConf x

  colToConf = show . fromEnum
