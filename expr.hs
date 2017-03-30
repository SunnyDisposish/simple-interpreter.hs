module Expr
  ( Expr(..)
  , Label
  , Lit
  , BinOp(..)
) where

type Lit = Int

data Expr
  = Lit Lit
  | Bin BinOp Expr Expr
  | Lam Label Expr
  | App Expr Expr
  | Var Label
  deriving Show

data BinOp = Add | Sub | Mul | Div
  deriving Show

type Label = String