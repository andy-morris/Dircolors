module P (
    P,
    entire,
    lexeme, symbol,
    word, word',
    pwhite,
  ) where

import Data.Char
import Text.Parsec
import Control.Applicative hiding ((<|>))
import Control.Monad

type P = Parsec String ()

entire :: P a -> P a
entire = (<* eof)

pcomment :: P ()
pcomment = void inner <?> "comment" where
  inner = do
    char '#'
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

quoted :: String -> String
quoted str = "\"" ++ str ++ "\""
