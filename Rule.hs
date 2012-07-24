module Rule where

import P
import Glob
import Attribute
import Text.Parsec
import Text.PrettyPrint.Free
import Control.Applicative hiding ((<|>), many)

data Rule = Rule [Glob] [Attribute]
  deriving Show

instance Pretty Rule where
    pretty (Rule gs attrs) =
      hsep (map pretty gs) <> ":" <+>
      hsep (punctuate "," $ map pretty attrs) <> "."
    prettyList = vsep . map pretty

prules :: P [Rule]
prules = many prule

prule :: P Rule
prule = liftA2 Rule (pglobs <* symbol ":") (pattributes <* symbol ".")
