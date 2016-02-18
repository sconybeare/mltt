module MLTT.Types.Abstraction where

import           MLTT.Types.Expr (Expr)

data Abstraction = Abs Variable Expr Expr
                 deriving (Show, Eq, Ord)
