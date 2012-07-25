module Glob (Glob(..), pglobs, pglob, generate) where

import P

import Text.Parsec
import Control.Applicative hiding (many)
import Data.Generics

data Glob = Lit Char | Branch [Glob] | Cat [Glob]
  deriving (Eq, Show, Typeable, Data)

pglobs :: P [Glob]
pglobs = many1 pglob

pglob :: P Glob
pglob = lexeme "glob" $ pglob' []

pglob', pclass, popt, plit :: String -> P Glob
pglob' n = fmap Cat . many1 $ choice [pclass n, popt n, plit n]

pclass n = Branch . map Lit . concat <$>
    between (char '[') (char ']') (many1 item)
    <?> "character class" where
  item = do c1 <- pNot (n ++ "]-")
            mc2 <- optionMaybe $ char '-' *> pNot (n ++ "]-")
            return $ maybe [c1] (\c2 -> [c1..c2]) mc2

popt n = Branch <$>
    between (char '{') (char '}') (part `sepBy1` char ',')
    <?> "Branch"
  where part = option (Cat []) $ pglob' (n ++ ",}")

plit n = Lit <$> pNot n <?> "normal character"

pesc :: P Char
pesc = char '\\' *> anyChar <?> "backslash escape"

pNot :: String -> P Char
pNot n = choice [pesc, noneOf $ n ++ "#:[{ \\"]


generate :: Glob -> [FilePath]
generate (Lit c) = return [c]
generate (Branch bs) = generate =<< bs
generate (Cat gs) = map concat . sequence $ map generate gs
