{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module MLTT.Types ( module Exported
                  , Context
                  , MonadInfer
                  ) where

import           MLTT.Types.Expr           as Exported
import           MLTT.Types.InferException as Exported
import           MLTT.Types.Variable       as Exported

import           Control.Monad.Catch       as Exported (MonadThrow, throwM)
import           Control.Monad.State.Class as Exported (MonadState, get, put)

import           Data.HashMap.Strict       (HashMap)

type Context = HashMap Variable (Expr, Maybe Expr)

type MonadInfer m = (MonadState Integer m, MonadThrow m)
