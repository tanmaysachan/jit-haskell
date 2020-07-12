module Syntax where

data Expr = Float Double
          | BinOp Op Expr Expr
          | Var String
          | Call String [Expr]
          | Function String [Expr] Expr
          | Extern String [Expr]
          deriving (Eq, Ord, Show)

data Op = Multiply | Divide | Plus | Minus deriving (Eq, Ord, Show)
