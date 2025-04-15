import Data.Foldable (traverse_)

import Lexer (tokenize)
import Parser (parse)

main :: IO ()
main = do
  contents <- readFile "test/example.amgl"
  let tokens = tokenize contents
  -- print tokens
  let ast = parse tokens
  case ast of
    Nothing -> print "Parse error somewhere"
    Just a -> mapM_ print a

-- main = traverse_ (print . parse . tokenize) ["let x = 3 + 2", "let y = 24", "let z = 3 * 2", "let a = 3 + 2 * 8", "let f(x) = x + 2", "let g(x, y) = x + y", "let f(x) = 3\nlet h(x,) = 2"]
