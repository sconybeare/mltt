module MLTT.Types.Expr where

import           Text.PrettyPrint.ANSI.Leijen

import           MLTT.Types.Variable

data Abstraction = Abs Variable Expr Expr
                 deriving (Show, Eq, Ord)

instance Pretty Abstraction where
  pretty (Abs v t e) = pretty v <> " : " <> pretty t <> " . " <> pretty e

data Expr = Var      !Variable
          | Universe !Word
          | Pi       !Abstraction
          | Lambda   !Abstraction
          | App      !Expr !Expr
          deriving (Show, Eq, Ord)

instance Pretty Expr where
  pretty (Var v)      = pretty v
  pretty (Universe n) = string $ show n
  pretty (Pi a)       = "Π " <> pretty a
  pretty (Lambda a)   = "λ " <> pretty a
  pretty (App e1 e2)  = pretty e1 <> " " <> pretty e2
