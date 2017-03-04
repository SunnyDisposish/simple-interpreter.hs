module Expr (
  Expr(..)
) where

data Expr
  = Add Int Int
  | Sub Int Int