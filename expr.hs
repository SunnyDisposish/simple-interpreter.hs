module Expr (
  Expr(..)
) where

data Expr
  = Add Int Int
  | Sub Int Int
  | Mul Int Int
  | Div Int Int