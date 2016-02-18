module MLTT.Types.Variable where

data Variable = StringVar String
              | GenSym String Integer
              | Dummy
              deriving (Show, Eq, Ord)
