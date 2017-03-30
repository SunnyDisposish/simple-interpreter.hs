module Parser (
  parseExpr
) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)

import Expr

type Op = Expr -> Expr -> Expr

parseExpr :: String -> Expr
parseExpr expr = case (parse parser "" expr) of
  Right e -> e
  Left err -> error $ show err

parser :: Parser Expr
parser = lambda <|> try app <|> expr

lambda :: Parser Expr
lambda = do
  char '\\'
  Var v <- var
  char '.'
  body <- parser
  return $ Lam v body

app :: Parser Expr
app = do
  lam <- parens parser
  arg <- parser
  return $ App lam arg

var :: Parser Expr
var = return . Var =<< many1 letter

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = whitespace (parens parser <|> literal <|> var)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

whitespace :: Parser a -> Parser a
whitespace = between spaces spaces

literal :: Parser Expr
literal = return . Lit =<< num

num :: Parser Int
num = return . read =<< many1 digit

addop :: Parser Op
addop = plus <|> minus

mulop :: Parser Op
mulop = times <|> dividedBy

plus :: Parser Op
plus = char '+' >> return (Bin Add)

minus :: Parser Op
minus = char '-' >> return (Bin Sub)

times :: Parser Op
times = char '*' >> return (Bin Mul)

dividedBy :: Parser Op
dividedBy = char '/' >> return (Bin Div)