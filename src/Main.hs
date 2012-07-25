import Rule
import Data.List
import qualified Data.Map as Map; import Data.Map (Map)
import Data.Typeable
import Text.Parsec (parse, ParseError)
import Control.Arrow
import Control.Monad.Error
import Control.Monad.State
import ReadArgs (readArgs)
import System.IO
import System.Exit

type M = ErrorT Err (State StyleMap)
type StyleMap = Map String Attributes

data Err
    = StyleNotFound String
    | ParseError ParseError
    | Other (Maybe String)

instance Show Err where
    show (StyleNotFound name) = concat ["style name ", name, " not found"]
    show (ParseError pe)      = show pe
    show (Other Nothing)      = "unknown error :("
    show (Other (Just s))     = s

instance Error Err where
    strMsg = Other . Just
    noMsg  = Other Nothing

newtype Filename = F {unF :: FilePath} deriving Typeable
instance Show Filename where show (F x) = x
instance Read Filename where readsPrec _ str = [(F str, "")]


main :: IO ()
main = do
    (inp :: Maybe Filename, outp :: Maybe Filename) <- readArgs
    input <- maybe getContents (readFile . unF) inp
    case main' input of
         Left e  -> hPrint stderr e >> exitFailure
         Right x -> maybe putStr (writeFile . unF) outp x

main' :: String -> Either String String
main' =
    (show +++ unlines . concat) .
    flip evalState Map.empty .
    runErrorT .
    (mapM output <=< parseRules)

parseRules :: String -> M [Rule]
parseRules str =
    case parse prules "<input>" str of
         Left e  -> throwError $ ParseError e
         Right x -> return x

output :: Rule -> M [String]
output (Term ts) = return $ map ("TERM " ++) ts
output (Rule gs attrs) = do
    conf <- toConf attrs
    return [ft ++ " " ++ conf | g <- gs, ft <- generate g]
output (Style name attrs) = modify (Map.insert name attrs) >> return []

toConf :: Attributes -> M String
toConf (Ref sty) = do
    attr <- gets $ Map.lookup sty
    case attr of
         Nothing -> throwError $ StyleNotFound sty
         Just a  -> toConf a
toConf (Inline attrs) = return $ toConfI attrs where
  toConfI [] = "00"
  toConfI xs = intercalate ";" $ map toConf' xs

  toConf' Bold      = "01"
  toConf' Underline = "04"
  toConf' Blink     = "05"
  toConf' Reverse   = "07"
  toConf' Conceal   = "08"
  toConf' (FG x)    = '3' : colToConf x
  toConf' (BG x)    = '4' : colToConf x

  colToConf = show . fromEnum
