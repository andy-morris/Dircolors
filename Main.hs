import Rule
import Text.Parsec

main :: IO ()
main =
    putStrLn . either show (unlines . concatMap output)
      . parse prules "<stdin>"
      =<< getContents

output :: Rule -> [String]
output (Term ts) = map ("TERM " ++) ts
output (Rule gs attrs) =
    [ft ++ " " ++ toConf attrs | g <- gs, ft <- generate g]
