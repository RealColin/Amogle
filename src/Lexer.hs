module Lexer (tokenize, Token(..)) where

import Text.Read (readMaybe)
import Data.Char (isLower, isUpper, isAlpha, isAlphaNum, isNumber)

data Token =
  TokenLet
  | TokenTypeIdent String
  | TokenValIdent String
  | TokenEquals
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDivide
  | TokenColon
  | TokenComma
  | TokenParenL
  | TokenParenR
  | TokenArrow
  | TokenInt Int
  | TokenDouble Double
  | TokenNL
  | TokenIndent
  | TokenUnknown
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':s) = tokenize s
tokenize ('\t':s) = TokenIndent : tokenize s
tokenize ('\n':s) = TokenNL : tokenize s
tokenize ('-':'>':s) = TokenArrow : tokenize s
tokenize (c:s) = getToken chunk : tokenize rest
  where
    (chunk, rest)
      | isAlpha c = takeChunk (c:s)
      | isNumber c = takeNumChunk (c:s)
      | otherwise = ([c], s)

takeChunk :: String -> (String, String)
takeChunk [] = ([], [])
takeChunk (c:s)
  | isAlphaNum c = (c:chunk, rest)
  | otherwise = ([], c:s)
    where (chunk, rest) = takeChunk s

takeNumChunk :: String -> (String, String)
takeNumChunk [] = ([], [])
takeNumChunk (c:s)
  | isNumber c = (c:chunk, rest)
  | c == '.' = (c:chunk, rest)
  | otherwise = ([], c:s)
    where (chunk, rest) = takeNumChunk s    

getToken :: String -> Token
getToken "let" = TokenLet
getToken "=" = TokenEquals
getToken "+" = TokenPlus
getToken "-" = TokenMinus
getToken "*" = TokenTimes
getToken "/" = TokenDivide
getToken ":" = TokenColon
getToken "," = TokenComma
getToken "(" = TokenParenL
getToken ")" = TokenParenR
getToken str
  | Just n <- readMaybe str :: Maybe Int = TokenInt n
  | Just d <- readMaybe str :: Maybe Double = TokenDouble d
  | isUpper c = TokenTypeIdent str
  | isLower c = TokenValIdent str
  | otherwise = TokenUnknown
    where c = head str
