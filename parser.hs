module Parser (
  parseExpr
) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)

import Expr

data Op
  = Plus 
  | Minus

parseExpr :: String -> Expr
parseExpr expr = case (parse parser "" expr) of
  Right e -> e
  _ -> error "invalid expression"

parser :: Parser Expr
parser = do
  e0 <- num
  spaces
  op' <- op
  spaces
  e1 <- num
  return $ case op' of
    Plus -> Add e0 e1
    Minus -> Sub e0 e1

num :: Parser Int
num = do
  n <- many1 digit
  return (read n)

op :: Parser Op
op = do
  plus <|> minus

plus :: Parser Op
plus = do
  void $ char '+'
  return Plus

minus :: Parser Op
minus = do
  void $ char '-'
  return Minus