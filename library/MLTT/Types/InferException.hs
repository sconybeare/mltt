module MLTT.Types.InferException where

import           Control.Monad.Catch
import           MLTT.Types.Expr
import           MLTT.Types.Variable

data InferException = IECouldNotEvaluate
                    | IETypeExpected
                    | IEFunctionExpected
                    | IESymbolNotFound Variable
                    | IEUnknownIdentifier Variable
                    | IEExpressionsNotEqual Expr Expr
                    deriving (Show)
                             -- Make a custom 'Show' instance to get
                             -- better error messages by default.

instance Exception InferException

throwCouldNotEvaluate    :: (MonadThrow m) => m a
throwTypeExpected        :: (MonadThrow m) => m a
throwFunctionExpected    :: (MonadThrow m) => m a
throwSymbolNotFound      :: (MonadThrow m) => Variable -> m a
throwUnknownIdentifier   :: (MonadThrow m) => Variable -> m a
throwExpressionsNotEqual :: (MonadThrow m) => Expr -> Expr -> m a
throwCouldNotEvaluate          = throwM   IECouldNotEvaluate
throwTypeExpected              = throwM   IETypeExpected
throwFunctionExpected          = throwM   IEFunctionExpected
throwSymbolNotFound      v     = throwM $ IESymbolNotFound v
throwUnknownIdentifier   v     = throwM $ IEUnknownIdentifier v
throwExpressionsNotEqual e1 e2 = throwM $ IEExpressionsNotEqual e1 e2
