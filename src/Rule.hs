module Rule (
    Rule(..), prules, prule,
    module Glob,
    module Attribute,
  ) where

import P
import Glob
import Attribute
import Text.Parsec
import Text.PrettyPrint.Free hiding (dot, char, colon)
import Control.Applicative hiding ((<|>), many)

data Rule = Term [String]
          | Style String Attributes
          | Rule [Glob] Attributes
  deriving Show

instance Pretty Rule where
    pretty (Term ts) = "term" <+> hsep (map pretty ts)
    pretty (Style name attrs) =
      "style" <+> pretty name <+> ":" <+>
      pretty attrs <> "."
    pretty (Rule gs attrs) =
      hsep (map pretty gs) <> ":" <+>
      pretty attrs <> "."
    prettyList = vsep . map pretty

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
