module MLTT where

import           Control.Monad
import           MLTT.Infer
import           MLTT.Parser
import           MLTT.Types

parseInfer :: (MonadThrow m) => Context -> String -> m Expr
parseInfer ctx = parseExpr >=> infer ctx

main :: IO ()
main = return ()
