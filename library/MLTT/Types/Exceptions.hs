{-# LANGUAGE FlexibleInstances #-}

module MLTT.Types.Exceptions ( throwPretty
                             , ParseException
                             , throwParseException
                             , InferException
                             , throwCouldNotEvaluate
                             , throwTypeExpected
                             , throwFunctionExpected
                             , throwSymbolNotFound
                             , throwUnknownIdentifier
                             , throwExpressionsNotEqual
                             ) where

import           Data.Typeable

import           Control.Monad.Catch
import           Text.PrettyPrint.ANSI.Leijen

import           MLTT.Types.Expr
import           MLTT.Types.Variable

--------------------------------------------------------------------------------
-- PrettyException -------------------------------------------------------------
--------------------------------------------------------------------------------

newtype PrettyException e = PrettyException e

instance (Pretty e) => Show (PrettyException e) where
  showsPrec _ (PrettyException d) = displayS $ renderCompact $ pretty d

instance (Typeable e, Pretty e) => Exception (PrettyException e)

throwPretty :: (Typeable e, Pretty e, MonadThrow m) => e -> m a
throwPretty = throwM . PrettyException

--------------------------------------------------------------------------------
-- ParseException --------------------------------------------------------------
--------------------------------------------------------------------------------

newtype ParseException = ParseException Doc

instance Pretty ParseException where
  pretty (ParseException d) = d

throwParseException :: (MonadThrow m) => Doc -> m a
throwParseException = throwPretty . ParseException

--------------------------------------------------------------------------------
-- InferException --------------------------------------------------------------
--------------------------------------------------------------------------------

data InferException = IECouldNotEvaluate
                    | IETypeExpected
                    | IEFunctionExpected
                    | IESymbolNotFound !Variable
                    | IEUnknownIdentifier !Variable
                    | IEExpressionsNotEqual !Expr !Expr

instance Pretty InferException where
  pretty IECouldNotEvaluate            = "Could not evaluate!"
  pretty IETypeExpected                = "Type expected!"
  pretty IEFunctionExpected            = "Function expected!"
  pretty (IESymbolNotFound v)          = "Symbol not found: " <> pretty v
  pretty (IEUnknownIdentifier v)       = "Unknown identifier: " <> pretty v
  pretty (IEExpressionsNotEqual e1 e2) = "Expressions not equal: "
                                         <> "'" <> pretty e1 <> "'"
                                         <> ", "
                                         <> "'" <> pretty e2 <> "'"

throwCouldNotEvaluate    :: (MonadThrow m) => m a
throwTypeExpected        :: (MonadThrow m) => m a
throwFunctionExpected    :: (MonadThrow m) => m a
throwSymbolNotFound      :: (MonadThrow m) => Variable -> m a
throwUnknownIdentifier   :: (MonadThrow m) => Variable -> m a
throwExpressionsNotEqual :: (MonadThrow m) => Expr -> Expr -> m a
throwCouldNotEvaluate          = throwPretty   IECouldNotEvaluate
throwTypeExpected              = throwPretty   IETypeExpected
throwFunctionExpected          = throwPretty   IEFunctionExpected
throwSymbolNotFound      v     = throwPretty $ IESymbolNotFound v
throwUnknownIdentifier   v     = throwPretty $ IEUnknownIdentifier v
throwExpressionsNotEqual e1 e2 = throwPretty $ IEExpressionsNotEqual e1 e2
