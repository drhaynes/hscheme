module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_-"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left err -> "No symbol match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  putStrLn (">")
  expr <- getLine
  putStrLn (readExpr expr)
