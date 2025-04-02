import Data.Foldable (traverse_)

import Lexer (tokenize)
import Parser (parse)

main :: IO ()
-- main = traverse_ (print . tokenize) ["let f = 32.4", "let g = 24", "let h = 0.1234"]
main = traverse_ (print . parse . tokenize) ["let f = 32.4", "let g = 24", "let h = 0.1234"]  
