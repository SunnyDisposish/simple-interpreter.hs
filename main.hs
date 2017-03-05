import Control.Monad

import Expr
import Parser

data Result
  = IntResult Int
  | FloatResult Float

showResult :: Result -> String
showResult (IntResult r) = show r
showResult (FloatResult r) = show r

eval :: Expr -> Result
eval (Add x y) = IntResult $ x + y
eval (Sub x y) = IntResult $ x - y
eval (Mul x y) = IntResult $ x * y
eval (Div x y) = FloatResult $ fromIntegral x / fromIntegral y

runProgram :: String -> Result
runProgram = eval . parseExpr

main :: IO()
main = forever $ do
  putStr "> "
  getLine >>= putStrLn . showResult . runProgram
