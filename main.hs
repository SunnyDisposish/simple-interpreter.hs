import Control.Monad

import Expr
import Parser

eval :: Expr -> Int
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Lit x) = x

runProgram :: String -> Int
runProgram = eval . parseExpr

main :: IO()
main = forever $ do
  putStr "> "
  getLine >>= putStrLn . show . runProgram