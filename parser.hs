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
parser = expr

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = whitespace (parens expr <|> literal)

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
plus = char '+' >> return Add

minus :: Parser Op
minus = char '-' >> return Sub

times :: Parser Op
times = char '*' >> return Mul

dividedBy :: Parser Op
dividedBy = char '/' >> return Div