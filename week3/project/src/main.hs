import System.Environment
import Text.Printf
import Simpl
import Simpl.Parser

parse _ = Skip

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      text <- readFile input
      let ast = parse text in
        printf "Out := %d\n" (run empty ast "Out")
    _ ->
      error "usage: simpl <file>"
