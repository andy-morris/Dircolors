module Glob (Glob(..), pglobs, pglob, generate) where

import P

import Data.String
import Text.Parsec
import Text.PrettyPrint.Free hiding (char)
import Control.Applicative hiding (many)
import Data.Generics

data Glob = Lit Char | Branch [Glob] | Cat [Glob]
  deriving (Eq, Show, Typeable, Data)

cleanup :: Glob -> Glob
cleanup = everywhere $ mkT cleanup' where
  cleanup' (Branch [x]) = x
  cleanup' (Branch xs)  = Branch $ concatMap branches xs
  cleanup' (Cat [x])    = x
  cleanup' (Cat xs)     = Cat $ concatMap cats xs
  cleanup' x            = x

  branches (Branch xs) = xs
  branches x           = [x]

  cats (Cat xs)        = xs
  cats x               = [x]

instance Pretty Glob where
  pretty = p' . cleanup where
    p' (Lit c) = fromString [c]
    p' (Branch gs)
      | all single gs = brackets . cat $ map p' gs
      | otherwise     = braces . cat . punctuate "," $ map p' gs
    p' (Cat xs) = cat $ map p' xs

    single (Lit _) = True
    single _       = False

pglobs :: P [Glob]
pglobs = many1 pglob

pglob :: P Glob
pglob = lexeme "glob" $ pglob' []

pglob', pclass, popt, plit :: [Char] -> P Glob
pglob' n = fmap Cat . many1 $ choice [pclass n, popt n, plit n]

pclass n = Branch . map Lit . concat <$>
    between (char '[') (char ']') (many1 item)
    <?> "character class" where
  item = do c1 <- pNot (n ++ "]-")
            mc2 <- optionMaybe $ char '-' *> pNot (n ++ "]-")
            return $ maybe [c1] (\c2 -> [c1..c2]) mc2

popt n = Branch <$>
    between (char '{') (char '}') (pglob' (n ++ ",}") `sepBy1` char ',')
    <?> "Branch"

plit n = Lit <$> pNot n <?> "normal character"

pesc :: P Char
pesc = char '\\' *> anyChar <?> "backslash escape"

pNot :: [Char] -> P Char
pNot n = choice [pesc, noneOf $ n ++ "#:[{ \\"]


generate :: Glob -> [FilePath]
generate (Lit c) = return [c]
generate (Branch bs) = generate =<< bs
generate (Cat gs) = map concat . sequence $ map generate gs
