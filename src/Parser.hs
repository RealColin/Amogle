{-# LANGUAGE LambdaCase #-}

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

-- type Params = [String] -- empty means plain old value, nonempty means function!
type ValName = String -- this has to start with a LOWERCASE letter

data Expr
  = IntLit Int
  | DoubleLit Double
  | Add Expr Expr
  | Sub Expr Expr
  | Ident String
  deriving (Show, Eq)

data Decl
  = ValDef ValName Expr
  | Empty
  deriving (Show, Eq)
    

-- parser functions!
satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken fn = Parser $ \case
  (t:rest) | fn t -> Just (t, rest)
  _ -> Nothing

eatToken :: Token -> Parser Token
eatToken t = satisfyToken (== t)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \input ->
  case runParser p1 input of
    Nothing -> runParser p2 input
    res -> res

parseValName :: Parser String
parseValName = Parser $ \case
    (TokenValIdent t:rest) -> Just (t, rest)
    _ -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \case
    (TokenInt t:rest) -> Just (t, rest)
    _ -> Nothing

parsePrimary :: Parser Expr
parsePrimary = i <|> e
  where
    i = IntLit <$> parseInt
    e = Ident <$> parseValName

parseAdd :: Parser Expr
parseAdd = do
  first <- parsePrimary
  _ <- eatToken TokenPlus
  Add first <$> parsePrimary

parseSub :: Parser Expr
parseSub = do
  first <- parsePrimary
  _ <- eatToken TokenMinus
  Sub first <$> parsePrimary

parseExpr :: Parser Expr
parseExpr = parseAdd <|> parseSub

parseValDef :: Parser Decl
parseValDef = do
  _ <- eatToken TokenLet
  name <- parseValName
  _ <- eatToken TokenEquals
  ValDef name <$> parseExpr

parseDecl :: Parser Decl
parseDecl = parseValDef

parse :: [Token] -> Maybe Decl
parse tokens = case runParser parseDecl tokens of
  Nothing -> Nothing
  Just (a, _) -> Just a
  
