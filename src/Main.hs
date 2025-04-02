import Text.Read (readMaybe)
import Data.Char (isLower, isUpper, isAlpha, isAlphaNum)
import Data.Foldable (traverse_)

data Token =
  TokenLet
  | TokenTypeIdent String
  | TokenValIdent String
  | TokenEquals
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
tokenize (c:s)
  | isAlpha c = getToken chunk : tokenize rest
  | otherwise = getToken [c] : tokenize s
    where (chunk, rest) = takeChunk (c:s)

takeChunk :: String -> (String, String)
takeChunk [] = ([], [])
takeChunk (c:s)
  | isAlphaNum c = (c:chunk, rest)
  | otherwise = ([], c:s)
    where (chunk, rest) = takeChunk s
    

getToken :: String -> Token
getToken "let" = TokenLet
getToken "=" = TokenEquals
getToken str
  | Just n <- readMaybe str :: Maybe Int = TokenInt n
  | Just d <- readMaybe str :: Maybe Double = TokenDouble d
  | isUpper c = TokenTypeIdent str
  | isLower c = TokenValIdent str
  | otherwise = TokenUnknown
    where c = head str

main :: IO ()
main = traverse_ (print . tokenize) ["let x3 = 1", "let x 3 = 1", "Joe joe"]  
