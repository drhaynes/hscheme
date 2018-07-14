module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_-"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  c <- oneOf "\\\"nrt" --- either backslash, double quatation mark, (n)ewline, (r)eturn, or (t)ab
  return $ case c of
    '\\' -> c
    '"'  -> c
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space" <|> string "NEWLINE" <|> string "SPACE")
          <|> do { c <- anyChar; notFollowedBy alphaNum ; return [c] }
  return $ Character $ case value of
    "space" -> ' '
    "SPACE" -> ' '
    "newline" -> '\n'
    "NEWLINE" -> '\n'
    otherwise -> (value !! 0)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

main :: IO ()
main = do
  putStrLn (">")
  expr <- getLine
  putStrLn (readExpr expr)

