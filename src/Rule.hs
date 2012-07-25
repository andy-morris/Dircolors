module Rule (
    Rule(..), prules, prule,
    module Glob,
    module Attribute,
  ) where

import P
import Glob
import Attribute
import Text.Parsec
import Control.Applicative hiding ((<|>), many)

data Rule = Term [String]
          | Style String Attributes
          | Rule [Glob] Attributes
  deriving Show

prules :: P [Rule]
prules = many $ choice [pterm, pstyle, prule]

pterm :: P Rule
pterm =
    fmap Term . between (reserved "term") dot $ many1 (identifier "terminal")

pstyle :: P Rule
pstyle = between (reserved "style") dot $
    liftA2 Style (identifier "style") (colon *> pattributes)

prule :: P Rule
prule = liftA2 Rule (pglobs <* colon) (pattributes <* dot)
