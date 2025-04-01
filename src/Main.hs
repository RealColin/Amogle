import Text.Read (readMaybe)

data Token =
  TokenLet
  | TokenIdent String
  | TokenEquals
  | TokenInt Int
  | TokenDouble Double
  | TokenNL
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize str = map getToken (words str)

getToken :: String -> Token
getToken "let" = TokenLet
getToken "=" = TokenEquals
getToken str
  | Just n <- readMaybe str :: Maybe Int = TokenInt n
  | Just d <- readMaybe str :: Maybe Double = TokenDouble d
  | otherwise = TokenIdent str

main :: IO ()
main = do
  let x = tokenize "let x = 3"
  print x
  let y = tokenize "let y = 3.0"
  print y
