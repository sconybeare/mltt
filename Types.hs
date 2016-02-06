{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
import Data.Word
import Control.Monad.State.Lazy
import Data.Functor.Identity

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

infer_type :: Context -> Expr -> StateT Integer (Either String) Expr
infer_type ctx (Var x) = lift $ lookup_ty x ctx
infer_type _ (Universe k) = lift $ Right $ Universe (k+1)
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
                mapStateT (Right . runIdentity) (subst [(x,e2)] t)

infer_universe :: Context -> Expr -> StateT Integer (Either String) Word
infer_universe ctx t = do
  u <- infer_type ctx t
  n <- normalize ctx u
  case n of
    Universe k -> return k
    _          -> lift $ Left "type expected"

infer_pi :: Context -> Expr -> StateT Integer (Either String) Abstraction
infer_pi ctx e = do
  t <- infer_type ctx e
  n <- normalize ctx t
  case n of
    Pi ab -> return ab
    _     -> lift $ Left "function expected"

check_equal :: Context -> Expr -> Expr -> StateT Integer (Either String) ()
check_equal ctx e1 e2 = do
  eq <- equal ctx e1 e2
  case eq of
    True  -> return ()
    False -> lift $
      Left ("expressions " ++ show e1 ++ " and " ++ show e2 ++ "are not equal")

normalize :: Context -> Expr -> StateT Integer (Either String) Expr
normalize ctx (Var x) = case lookup x ctx of
  Nothing -> lift $ Left ("unknown identifier " ++ show x)
  Just (_,Nothing) -> return $ Var x
  Just (_,Just val) -> return $ val
normalize ctx (App e1 e2) = do
  e2_n <- normalize ctx e2
  e1_n <- normalize ctx e1
  case e1_n of
    Lambda (Abs x _ e1') -> mapStateT (Right . runIdentity) $
                            subst [(x,e2)] e1'
    e1_n -> return $ App e1_n e2_n
normalize ctx (Universe k) = return $ Universe k
normalize ctx (Pi ab) = Pi <$> normalize_abstraction ctx ab
normalize ctx (Lambda ab) = Lambda <$> normalize_abstraction ctx ab

normalize_abstraction :: Context ->
                         Abstraction ->
                         StateT Integer (Either String) Abstraction
normalize_abstraction ctx (Abs x t e) = do
  t' <- normalize ctx t
  Abs x t <$> normalize (extend x t Nothing ctx) e

equal :: Context -> Expr -> Expr -> StateT Integer (Either String) Bool
equal ctx e1 e2 = do
  e1' <- normalize ctx e1
  e2' <- normalize ctx e2
  mapStateT (Right . runIdentity) $ equal' e1' e2'

equal' :: Expr -> Expr -> State Integer Bool
equal' (Var x1) (Var x2) = return $ x1 == x2
equal' (App e11 e12) (App e21 e22) = (&&) <$> equal' e11 e21 <*> equal' e12 e22
equal' (Universe k1) (Universe k2) = return $ k1 == k2
equal' (Pi a1) (Pi a2) = equal_abstraction a1 a2
equal' (Lambda a1) (Lambda a2) = equal_abstraction a1 a2
equal' _ _ = return False

equal_abstraction :: Abstraction -> Abstraction -> State Integer Bool
equal_abstraction (Abs x t1 e1) (Abs y t2 e2) =
                (&&) <$> equal' t1 t2 <*> (equal' e1 =<< subst [(y,Var x)] e2)
