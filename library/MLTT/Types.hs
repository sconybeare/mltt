{-# OPTIONS_GHC -Wall #-}

module MLTT.Types where

import           Data.Word

data Variable = StringVar String
              | GenSym String Integer
              | Dummy
              deriving (Show, Eq, Ord)

data Abstraction = Abs Variable Expr Expr
                 deriving (Show, Eq, Ord)

data Expr = Var Variable
          | Universe Word
          | Pi Abstraction
          | Lambda Abstraction
          | App Expr Expr
          deriving (Show, Eq, Ord)

type Context = [(Variable, (Expr, Maybe Expr))]
