{-# LANGUAGE LambdaCase #-}

module Parser (parse) where

import Lexer (Token(..))

-- Defining the Parser

newtype Parser a = Parser {
  runParser :: [Token] -> Maybe (a, [Token])
}

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

-- Basic Parser Functions

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

-- chainl1 function
(<.>) :: Parser a -> Parser (a -> a -> a) -> Parser a
p <.> op = do
  x <- p
  rest x
  where
    rest acc = (do
      f <- op
      y <- p
      rest (f acc y))
      <|> return acc

-- Defining AST types     

type Params = [String] -- empty means plain old value, nonempty means function!
type ValName = String -- this has to start with a LOWERCASE letter

data Expr
  = IntLit Int
  | DoubleLit Double
  | Add Expr Expr
  | Sub Expr Expr
  | Ident String
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

data Decl
  = ValDef ValName Params Expr
  | Empty
  deriving (Show, Eq)

-- Expression Parsing Functions

parseValName :: Parser String
parseValName = Parser $ \case
    (TokenValIdent t:rest) -> Just (t, rest)
    _ -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \case
    (TokenInt t:rest) -> Just (t, rest)
    _ -> Nothing

parseParams :: Parser Params
parseParams = do
  parseParen <|> pure []
    where
      parseParen = do
        _ <- eatToken TokenParenL
        param <- parseValName
        _ <- eatToken TokenParenR
        pure [param]
  
parseAdd :: Parser (Expr -> Expr -> Expr)
parseAdd = eatToken TokenPlus >> pure Add

parseSub :: Parser (Expr -> Expr -> Expr)
parseSub = eatToken TokenMinus >> pure Sub

parseMul :: Parser (Expr -> Expr -> Expr)
parseMul = eatToken TokenTimes >> pure Mul

parseDiv :: Parser (Expr -> Expr -> Expr)
parseDiv = eatToken TokenDivide >> pure Div

parsePrimary :: Parser Expr
parsePrimary = i <|> e
  where
    i = IntLit <$> parseInt
    e = Ident <$> parseValName

parseSecondary :: Parser Expr
parseSecondary = parsePrimary <.> (parseAdd <|> parseSub)

parseTertiary :: Parser Expr
parseTertiary = parseSecondary <.> (parseMul <|> parseDiv)

parseExpr :: Parser Expr
parseExpr = parseTertiary

-- Declaration Parsing Functions
    
parseValDef :: Parser Decl
parseValDef = do
  _ <- eatToken TokenLet
  name <- parseValName
  params <- parseParams
  _ <- eatToken TokenEquals
  ValDef name params <$> parseExpr

parseDecl :: Parser Decl
parseDecl = parseValDef

-- Main parse function

parse :: [Token] -> Maybe Decl
parse tokens = case runParser parseDecl tokens of
  Nothing -> Nothing
  Just (a, _) -> Just a
  
