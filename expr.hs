module Expr (
  Expr(..)
) where

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Lit Int
  deriving Show