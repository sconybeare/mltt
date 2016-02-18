{-# OPTIONS_GHC -Wall #-}

module Types where

import           Control.Monad.State.Lazy
import           Data.Functor.Identity
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
