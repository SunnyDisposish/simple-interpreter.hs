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
main = do
  putStrLn $ showResult $ runProgram "2 + 2"
  putStrLn $ showResult $ runProgram "2 - 2"
  putStrLn $ showResult $ runProgram "2 * 3"
  putStrLn $ showResult $ runProgram "12 / 2"
