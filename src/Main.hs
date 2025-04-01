import Text.Read (readMaybe)

data Token =
  TokenLet
  | TokenIdent String
  | TokenEquals
  | TokenInt Int
  | TokenNL
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize str = map getToken (words str)

getToken :: String -> Token
getToken "let" = TokenLet
getToken "=" = TokenEquals
getToken str =
  case readMaybe str :: Maybe Int of
    Just n -> TokenInt n
    Nothing -> TokenIdent str

main :: IO ()
main = do
  let x = tokenize "let x = 3"
  print x
