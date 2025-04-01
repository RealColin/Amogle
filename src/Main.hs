import Text.Read (readMaybe)
import Data.Char (isLower, isUpper, isAlpha)

data Token =
  TokenLet
  | TokenTypeIdent String
  | TokenValIdent String
  | TokenEquals
  | TokenInt Int
  | TokenDouble Double
  | TokenNL
  | TokenIndent
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':s) = tokenize s
tokenize ('\t':s) = TokenIndent : tokenize s
tokenize ('\n':s) = TokenNL : tokenize s
tokenize (c:s)
  | isAlpha c = getToken chunk : tokenize rest
  | otherwise = TokenLet : tokenize s
    where (chunk, rest) = takeChunk (c:s)

takeChunk :: String -> (String, String)
takeChunk [] = ([], [])
takeChunk (c:s)
  | isAlpha c = (c:chunk, rest)
  | otherwise = ([], s)
    where (chunk, rest) = takeChunk s
    

getToken :: String -> Token
getToken "let" = TokenLet
getToken "=" = TokenEquals
getToken str
  | Just n <- readMaybe str :: Maybe Int = TokenInt n
  | Just d <- readMaybe str :: Maybe Double = TokenDouble d
  | otherwise = TokenValIdent str

main :: IO ()
main = do
  let x = tokenize "let x = 3"
  print x
  let y = tokenize "let y = 3\nlet z = 4"
  print y
  let z = tokenize "letz = 3"
  print z
  
