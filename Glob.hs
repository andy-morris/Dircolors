module Glob (Glob(..), glob, pglob, generate) where

import Data.String
import Text.Parsec
import Text.PrettyPrint.Free hiding (char)
import Control.Applicative
import Data.Generics

data Glob = Lit Char | Branch [Glob] | Cat [Glob]
  deriving (Eq, Show, Typeable, Data)

glob :: String -> Either String Glob
glob str =
    case parse pglob "<glob>" str of
         Left err -> Left $ show err
         Right x  -> Right $ cleanup x

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
  pretty (Lit c) = fromString [c]
  pretty (Branch gs)
    | all single gs = brackets . cat $ map pretty gs
    | otherwise     = braces . cat . punctuate "," $ map pretty gs
  pretty (Cat xs) = cat $ map pretty xs

single :: Glob -> Bool
single (Lit _) = True
single _       = False

type P = Parsec String ()

pglob :: P Glob
pglob = pglob' [] <* eof

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
pNot n = choice [pesc, anyChar `butNot` (n ++ "#:[{ \\")] where
  p `butNot` cs = try $ do
    x <- p
    if x `elem` cs then unexpected [x] else return x


generate :: Glob -> [FilePath]
generate (Lit c) = return [c]
generate (Branch bs) = generate =<< bs
generate (Cat gs) = map concat . sequence $ map generate gs
