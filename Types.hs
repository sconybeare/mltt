{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
import Data.Word
import Control.Monad.State.Lazy

data Abstraction = Abs Variable Expr Expr
  deriving (Show,Eq,Ord)

data Expr = Var Variable
          | Universe Word
          | Pi Abstraction
          | Lambda Abstraction
          | App Expr Expr
  deriving (Show,Eq,Ord)

data Variable = StringVar String
              | GenSym String Integer
              | Dummy
  deriving (Show,Eq,Ord)

refresh :: Variable -> State Integer Variable
refresh (StringVar x) = do
  k <- get
  put $ k+1
  return $ GenSym x k
refresh (GenSym x _) = refresh (StringVar x)
refresh Dummy = refresh (StringVar "_")

subst :: [(Variable, Expr)] -> Expr -> State Integer Expr
subst s (Var x)      = return $ maybe (Var x) id (lookup x s)
subst _ (Universe k) = return $ Universe k
subst s (Pi abs)     = Pi     <$> subst_abstraction s abs
subst s (Lambda abs) = Lambda <$> subst_abstraction s abs
subst s (App e1 e2)  = App <$> subst s e1 <*> subst s e2

subst_abstraction :: [(Variable,Expr)]
                  -> Abstraction
                  -> State Integer Abstraction
subst_abstraction s (Abs x t e) = do
  x' <- refresh x
  t' <- subst s t
  e' <- subst ((x,Var x'):s) e
  return $ Abs x' t' e'

type Context = [(Variable, (Expr, Maybe Expr))]

lookup_ty :: Variable -> Context -> Either String Expr
lookup_ty x ctx = case lookup x ctx of
  Just (t,_) -> Right t
  Nothing    -> Left $ "Symbol " ++ show x ++ " not found."

lookup_value :: Variable -> Context -> Either String Expr
lookup_value x ctx = case lookup x ctx of
  Just (_,Just v)  -> Right v
  Just (_,Nothing) -> Left $ "Could not evaluate expression."
  othing          -> Left $ "Symbol " ++ show x ++ " not found."


extend :: Variable -> Expr -> Maybe Expr -> Context -> Context
extend x t v ctx = (x, (t, v)) : ctx

infer_type :: Context -> Expr -> StateT (Either String) Expr
infer_type ctx (Var x) = lookup_ty x ctx
infer_type _ (Universe k) = Right $ Universe (k+1)
infer_type ctx (Pi (Abs x t1 t2)) = 
                ((Universe . ) . max) <$>
                (infer_universe ctx t1) <*>
                (infer_universe (extend x t1 Nothing ctx) t2)
infer_type ctx (Lambda (Abs x t e)) = do
                _ <- infer_universe ctx t
                te <- infer_type (extend x t Nothing ctx) e
                return . Pi $ Abs x t te
infer_type ctx (App e1 e2) = do
                Abs x s t <- infer_pi ctx e1
                te <- infer_type ctx e2
                _ <- check_equal ctx s te
                _ $ subst [(x,e2)] t

infer_universe :: Context -> Expr -> Either String Word
infer_universe = undefined

infer_pi :: Context -> Expr -> Either String Abstraction
infer_pi = undefined

check_equal :: Context -> Expr -> Expr -> Either String ()
check_equal = undefined
