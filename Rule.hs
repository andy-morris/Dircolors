module Rule (
    Rule(..), prules, prule,
    module Glob,
    module Attribute,
  ) where

import P
import Glob
import Attribute
import Data.Char
import Text.Parsec
import Text.PrettyPrint.Free hiding (dot)
import Control.Applicative hiding ((<|>), many)

data Rule = Term [String]
          | Rule [Glob] [Attribute]
  deriving Show

instance Pretty Rule where
    pretty (Term ts) = "term" <+> hsep (map pretty ts)
    pretty (Rule gs attrs) =
      hsep (map pretty gs) <> ":" <+>
      hsep (punctuate "," $ map pretty attrs) <> "."
    prettyList = vsep . map pretty

prules :: P [Rule]
prules = many $ pterm <|> prule

pterm :: P Rule
pterm = fmap Term . between (word "term" ()) dot $ many1 identifier where
  identifier = lexeme "term identifier" . many1 $ satisfy idChar
  idChar c = not (isSpace c) && c /= '.'

prule :: P Rule
prule = liftA2 Rule (pglobs <* symbol ":") (pattributes <* symbol ".")

dot :: P ()
dot = symbol "."
