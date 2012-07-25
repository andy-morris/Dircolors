module P (
    P,
    entire, pwhite,
    lexeme, word, word', identifier,
    symbol, reserved, dot, comma, equals, colon,
  ) where

import Data.Char
import qualified Data.Set as Set; import Data.Set (Set)
import Text.Parsec
import Control.Applicative hiding ((<|>))
import Control.Monad

type P = Parsec String ()

entire :: P a -> P a
entire = (<* eof)

pcomment :: P ()
pcomment = void inner <?> "comment" where
  inner = do
    _ <- char '#'
    skipMany $ noneOf "\n"
    void (char '\n') <|> eof

pwhite :: P ()
pwhite = skipMany (void (satisfy isSpace) <|> pcomment) <?> "whitespace"

lexeme :: String -> P a -> P a
lexeme str = (<?> str) . between pwhite pwhite . try

symbol :: String -> P ()
symbol str = void . lexeme (quoted str) $ string str

word :: String -> a -> P a
word str x = lexeme (quoted str) $ do
    str' <- many1 $ satisfy isLetter
    if map toLower str == map toLower str'
       then return x
       else unexpected $ quoted str'

word' :: Show a => a -> P a
word' x = let sx = show x in word sx x

reserved :: String -> P ()
reserved = flip word ()

identifier :: String -> P String
identifier name = lexeme (name ++ " identifier") $ do
    ident <- many1 $ esc <|> satisfy idChar
    if map toLower ident `Set.member` reservedWords
       then unexpected $ quoted ident
       else return ident
  where esc = char '\\' *> anyChar
        idChar c = not (isSpace c) && c `notElem` ".:"

reservedWords :: Set String
reservedWords = Set.fromList $ words
  "term style bold underline blink reverse conceal fg bg \
  \black red green yellow blue magenta cyan white reset"

dot, comma, equals, colon :: P ()
dot    = symbol "."
comma  = symbol ","
equals = symbol "="
colon  = symbol ":"

quoted :: String -> String
quoted str = "\"" ++ str ++ "\""
