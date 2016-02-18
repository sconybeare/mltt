module MLTT.Types.Expr where

import           MLTT.Types.Variable

data Abstraction = Abs Variable Expr Expr
                 deriving (Show, Eq, Ord)

data Expr = Var Variable
          | Universe Word
          | Pi Abstraction
          | Lambda Abstraction
          | App Expr Expr
          deriving (Show, Eq, Ord)
