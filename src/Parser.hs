module Parser (parse) where

import Lexer (Token(..))

newtype Parser a = Parser {
  runParser :: [Token] -> Maybe (a, [Token])
}

-- kinda understand these!

instance Functor Parser where
  fmap f p = Parser (\input ->
    case runParser p input of
      Nothing -> Nothing
      Just (x, rest) -> Just (f x, rest))

instance Applicative Parser where
  pure a = Parser (\input -> Just(a, input))
  pfab <*> pa = Parser (\input ->
    case runParser pfab input of
      Nothing -> Nothing
      Just (f, rest) ->
        case runParser pa rest of
          Nothing -> Nothing
          Just (a, rest') -> Just(f a, rest'))

instance Monad Parser where
  return = pure
  pa >>= fapb = Parser (\input ->
    case runParser pa input of
      Nothing -> Nothing
      Just (x, rest) -> runParser (fapb x) rest)

type Params = [String] -- empty means plain old value, nonempty means function!
type ValName = String -- this has to start with a LOWERCASE letter

data Expr
  = IntLit Int
  | DoubleLit Double
  | Add Expr Expr
  | Sub Expr Expr
  | ValDef ValName Params Expr
  deriving (Show, Eq)

-- parser functions!

parseLExpr :: Parser Expr
parseLExpr = Parser (\input -> Just (IntLit 3, input)) -- placeholder

parseRExpr :: Parser Expr
parseRExpr = Parser (\input -> Just (IntLit 4, input)) -- placeholder


parse :: [Token] -> Maybe Expr
parse tokens = Just (IntLit 3)
