import Data.Foldable (traverse_)

import Lexer (tokenize)
import Parser (parse)

main :: IO ()
-- main = traverse_ (print . tokenize) ["let f = 32.4", "let g = 24", "let h = 0.1234"]
-- main = do
--   let str = "let x = 3 * 2 + 2"
--   let toks = tokenize str
--   print toks
--   let yea = parse toks
--   case yea of
--     Just a -> print a
--     Nothing -> print "Must have been a parse error"
main = traverse_ (print . parse . tokenize) ["let x = 3 + 2", "let y = 24", "let z = 3 * 2", "let a = 3 + 2 * 8", "let f(x) = x + 2", "let g(x, y) = x + y", "let h(,) = 2"]
