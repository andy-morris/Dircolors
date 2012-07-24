import Rule
import Text.Parsec
import Text.PrettyPrint.Free

main =
    putStrLn . either show (show . pretty) . parse prules "<stdin>"
      =<< getContents
