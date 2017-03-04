import Expr
import Parser


eval :: Expr -> Int
eval (Add x y) = x + y
eval (Sub x y) = x - y

runProgram :: String -> Int
-- eval(parseExpr())
runProgram = eval . parseExpr

main :: IO()
main = do
  -- print (runProgram "2 + 2")
  print $ runProgram "2 - 2"