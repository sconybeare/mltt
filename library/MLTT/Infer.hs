{-# LANGUAGE QuasiQuotes #-}

module MLTT.Infer where

import           Control.Monad.State.Lazy
import           Data.Functor.Identity
import           Data.Maybe
import           Data.String.Here

import           MLTT.Types

refresh :: Variable -> State Integer Variable
refresh (StringVar x) = do k <- get
                           put $ k + 1
                           return $ GenSym x k
refresh (GenSym x _)  = refresh (StringVar x)
refresh Dummy         = refresh (StringVar "_")

subst :: [(Variable, Expr)] -> Expr -> State Integer Expr
subst s (Var x)      = return $ fromMaybe (Var x) $ lookup x s
subst _ (Universe k) = return $ Universe k
subst s (Pi a)       = Pi     <$> substAbstraction s a
subst s (Lambda a)   = Lambda <$> substAbstraction s a
subst s (App e1 e2)  = App    <$> subst s e1 <*> subst s e2

substAbstraction :: [(Variable, Expr)]
                 -> Abstraction
                 -> State Integer Abstraction
substAbstraction s (Abs x t e) = do x' <- refresh x
                                    t' <- subst s t
                                    e' <- subst ((x, Var x') : s) e
                                    return $ Abs x' t' e'

lookupType :: Variable -> Context -> Either String Expr
lookupType x ctx = case lookup x ctx
                   of Just (t, _) -> Right t
                      Nothing     -> Left [i|Symbol ${x} not found|]

lookupValue :: Variable -> Context -> Either String Expr
lookupValue x ctx
  | Just (_, Just v)  <- lookup x ctx = Right v
  | Just (_, Nothing) <- lookup x ctx = Left [i|Could not evaluate expression|]
  | Nothing           <- lookup x ctx = Left [i|Symbol ${x} not found|]

extend :: Variable -> Expr -> Maybe Expr -> Context -> Context
extend x t v ctx = (x, (t, v)) : ctx

inferType :: Context -> Expr -> StateT Integer (Either String) Expr
inferType ctx (Var x)              = lift $ lookupType x ctx
inferType _   (Universe k)         = lift $ Right $ Universe (k + 1)
inferType ctx (Pi (Abs x t1 t2))   = inferTypeHelperPi ctx x t1 t2
inferType ctx (Lambda (Abs x t e)) = inferTypeHelperLambda ctx x t e
inferType ctx (App e1 e2)          = inferTypeHelperApp ctx e1 e2

inferTypeHelperPi :: Context -> Variable -> Expr -> Expr
                  -> StateT Integer (Either String) Expr
inferTypeHelperPi ctx x t1 t2 = do
  a <- inferUniverse ctx t1
  b <- inferUniverse (extend x t1 Nothing ctx) t2
  return $ Universe $ max a b

inferTypeHelperLambda :: Context -> Variable -> Expr -> Expr
                      -> StateT Integer (Either String) Expr
inferTypeHelperLambda ctx x t e = do inferUniverse ctx t
                                     Pi . Abs x t
                                       <$> inferType (extend x t Nothing ctx) e

inferTypeHelperApp :: Context -> Expr -> Expr
                   -> StateT Integer (Either String) Expr
inferTypeHelperApp ctx e1 e2 = do Abs x s t <- inferPi ctx e1
                                  te <- inferType ctx e2
                                  checkEqual ctx s te
                                  mapStateT
                                    (Right . runIdentity)
                                    (subst [(x, e2)] t)

inferUniverse :: Context -> Expr -> StateT Integer (Either String) Word
inferUniverse ctx t = do u <- inferType ctx t
                         n <- normalize ctx u
                         case n of
                           Universe k -> return k
                           _          -> lift $ Left "type expected"

inferPi :: Context -> Expr -> StateT Integer (Either String) Abstraction
inferPi ctx e = do t <- inferType ctx e
                   n <- normalize ctx t
                   case n of
                     Pi ab -> return ab
                     _     -> lift $ Left "function expected"

checkEqual :: Context -> Expr -> Expr -> StateT Integer (Either String) ()
checkEqual ctx e1 e2 = do
  eq <- equal ctx e1 e2
  when eq $ lift $ Left [i|expressions ${e1} and ${e2} are not equal|]

normalize :: Context -> Expr -> StateT Integer (Either String) Expr
normalize ctx (Var x)
  | Nothing           <- lookup x ctx = lift $ Left [i|unknown identifier ${x}|]
  | Just (_, Nothing) <- lookup x ctx = return $ Var x
  | Just (_, Just v)  <- lookup x ctx = return v
normalize ctx (App e1 e2) = do
  norm2 <- normalize ctx e2
  norm1 <- normalize ctx e1
  case norm1 of
    Lambda (Abs x _ e1') -> mapStateT (Right . runIdentity)
                            $ subst [(x, e2)] e1'
    _                    -> return $ App norm1 norm2
normalize ctx (Universe k) = return $ Universe k
normalize ctx (Pi ab)      = Pi     <$> normalizeAbstraction ctx ab
normalize ctx (Lambda ab)  = Lambda <$> normalizeAbstraction ctx ab

normalizeAbstraction :: Context
                     -> Abstraction
                     -> StateT Integer (Either String) Abstraction
normalizeAbstraction ctx (Abs x t e) = do
  t' <- normalize ctx t
  Abs x t <$> normalize (extend x t Nothing ctx) e

equal :: Context -> Expr -> Expr -> StateT Integer (Either String) Bool
equal ctx e1 e2 = do
  e1' <- normalize ctx e1
  e2' <- normalize ctx e2
  mapStateT (Right . runIdentity) $ equal' e1' e2'

equal' :: Expr -> Expr -> State Integer Bool
equal' (Var x1)      (Var x2)      = return $ x1 == x2
equal' (Universe k1) (Universe k2) = return $ k1 == k2
equal' (App e11 e12) (App e21 e22) = (&&) <$> equal' e11 e21 <*> equal' e12 e22
equal' (Pi a1)       (Pi a2)       = equalAbstraction a1 a2
equal' (Lambda a1)   (Lambda a2)   = equalAbstraction a1 a2
equal' _             _             = return False

equalAbstraction :: Abstraction -> Abstraction -> State Integer Bool
equalAbstraction (Abs x t1 e1) (Abs y t2 e2)
  = (&&) <$> equal' t1 t2 <*> (equal' e1 =<< subst [(y, Var x)] e2)
