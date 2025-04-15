{-# LANGUAGE LambdaCase #-}

module Parser (parse) where

import Control.Applicative (Alternative(..))
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

instance Alternative Parser where
  empty = Parser $ const Nothing
  pa <|> pb = Parser $ \input ->
    case runParser pa input of
      Nothing -> runParser pb input
      res -> res

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

eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing

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

(|-|) :: Parser a -> Parser s -> Parser [a]
p |-| sep = joe []
  where
    joe rest = (do
      next <- p
      _ <- sep
      joe (next:rest)
      ) <|> (do
        next <- p
        pure $ reverse (next:rest)
      )

-- Defining AST types     

type Params = [String] -- empty means plain old value, nonempty means function!
type ValName = String -- this has to start with a LOWERCASE letter
type TypeName = String

data Expr
  = IntLit Int
  | DoubleLit Double
  | Add Expr Expr
  | Sub Expr Expr
  | Ident String
  | Mul Expr Expr
  | Div Expr Expr
  | Call Expr [Expr]
  deriving (Show, Eq)

data Decl
  = ValDef ValName Params Expr
  | TypeSpec ValName [TypeName] TypeName
  | Empty
  deriving (Show, Eq)

-- Expression Parsing Functions

parseValName :: Parser String
parseValName = Parser $ \case
  (TokenValIdent t:rest) -> Just (t, rest)
  _ -> Nothing

parseTypeName :: Parser TypeName
parseTypeName = Parser $ \case
  (TokenTypeIdent t:rest) -> Just (t, rest)
  _ -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \case
  (TokenInt t:rest) -> Just (t, rest)
  _ -> Nothing

parseParams :: Parser Params
parseParams = parseParen <|> pure []
  where
    parseParen = do
      _ <- eatToken TokenParenL
      params <- parseValName |-| eatToken TokenComma
      _ <- eatToken TokenParenR
      pure params
  
parseAdd :: Parser (Expr -> Expr -> Expr)
parseAdd = eatToken TokenPlus >> pure Add

parseSub :: Parser (Expr -> Expr -> Expr)
parseSub = eatToken TokenMinus >> pure Sub

parseMul :: Parser (Expr -> Expr -> Expr)
parseMul = eatToken TokenTimes >> pure Mul

parseDiv :: Parser (Expr -> Expr -> Expr)
parseDiv = eatToken TokenDivide >> pure Div

parseFuncCall :: Parser Expr
parseFuncCall = do
  name <- parseValName
  _ <- eatToken TokenParenL
  exprs <- parseExpr |-| eatToken TokenComma
  _ <- eatToken TokenParenR
  pure $ Call (Ident name) exprs

parsePrimary :: Parser Expr
parsePrimary = i <|> f <|> e
  where
    i = IntLit <$> parseInt
    f = parseFuncCall
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

parseTypeSpec :: Parser Decl
parseTypeSpec = first <|> second
  where
    first = do
      ident <- parseValName
      _ <- eatToken TokenColon
      paramType <- parseTypeName |-| eatToken TokenComma
      _ <- eatToken TokenArrow
      TypeSpec ident paramType <$> parseTypeName
    second = do
      ident <- parseValName
      _ <- eatToken TokenColon
      TypeSpec ident [] <$> parseTypeName

parseDecl :: Parser Decl
parseDecl = parseValDef <|> parseTypeSpec

parseDecls :: Parser [Decl]
parseDecls = parseDecl |-| some (eatToken TokenNL)

-- Main parse function

parseProg :: Parser [Decl]
parseProg = do
  decls <- parseDecls
  _ <- many (eatToken TokenNL)
  _ <- eof
  pure decls

parse :: [Token] -> Maybe [Decl]
parse tokens = case runParser parseProg tokens of
  Nothing -> Nothing
  Just (a, rest) ->
    if null rest
      then Just a
      else error ("Un-eaten tokens: " ++ show rest)
