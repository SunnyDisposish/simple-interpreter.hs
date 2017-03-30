import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Expr
import Parser

type Env = Map Label Value

emptyEnv :: Env
emptyEnv = M.empty

data Value
  = VInt Int
  | VClosure Env Label Expr
  deriving Show

eval :: Env -> Expr -> Value
eval env (Bin op x y) = (evalBin op) (eval env x) (eval env y)
eval env (Lit x)   = VInt x
eval env (Lam x y) = VClosure env x y
eval env (App lam arg) = let VClosure env' param body = eval env lam
                         in eval (M.insert param (eval env arg) env') body
eval env (Var x)   = fromJust $ M.lookup x env

evalBin :: BinOp -> Value -> Value -> Value
evalBin op (VInt x) (VInt y) = case op of
  Add -> VInt $ x + y
  Sub -> VInt $ x - y
  Mul -> VInt $ x * y
  Div -> VInt $ x `div` y

runProgram :: String -> Value
runProgram = eval emptyEnv . parseExpr

main :: IO ()
main = forever $ do
  putStr "> "
  getLine >>= print . runProgram