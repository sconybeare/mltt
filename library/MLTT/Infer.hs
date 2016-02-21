{-# LANGUAGE FlexibleContexts #-}

module MLTT.Infer where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State.Strict (evalStateT)

import           Control.Lens               hiding (Context)
import           Control.Lens.At

import           Data.Functor.Identity
import           Data.Maybe

import qualified Data.HashMap.Strict        as HM

import           MLTT.Types

type Map k v = HM.HashMap k v

infer :: (MonadThrow m) => Context -> Expr -> m Expr
infer ctx = flip evalStateT 0 . inferType ctx

refresh :: (MonadInfer m) => Variable -> m Variable
refresh (NamedVar v) = do k <- get
                          put $ k + 1
                          return $ GenSym v k
refresh (GenSym v _) = refresh (NamedVar v)
refresh Dummy        = refresh (NamedVar "_")

subst :: (MonadInfer m) => Map Variable Expr -> Expr -> m Expr
subst s (Var v)      = return $ fromMaybe (Var v) $ s ^. at v
subst _ (Universe k) = return $ Universe k
subst s (Pi a)       = Pi     <$> substAbs s a
subst s (Lambda a)   = Lambda <$> substAbs s a
subst s (App e1 e2)  = App    <$> subst s e1 <*> subst s e2

substAbs :: (MonadInfer m) => Map Variable Expr -> Abstraction -> m Abstraction
substAbs s (Abs v t e) = do v' <- refresh v
                            t' <- subst s t
                            e' <- subst (HM.insert v (Var v') s) e
                            return $ Abs v' t' e'

lookupType :: (MonadInfer m) => Variable -> Context -> m Expr
lookupType v ctx = case ctx ^. at v
                   of Just (t, _) -> return t
                      Nothing     -> throwSymbolNotFound v

lookupValue :: (MonadInfer m) => Variable -> Context -> m Expr
lookupValue v ctx = helper $ ctx ^. at v
  where
    helper (Just (_, Just e))  = return e
    helper (Just (_, Nothing)) = throwCouldNotEvaluate
    helper Nothing             = throwSymbolNotFound v

extend :: Variable -> Expr -> Maybe Expr -> Context -> Context
extend v t x ctx = HM.insert v (t, x) ctx

inferType :: (MonadInfer m) => Context -> Expr -> m Expr
inferType ctx (Var x)              = lookupType x ctx
inferType _   (Universe k)         = return $ Universe $ k + 1
inferType ctx (Pi (Abs x t1 t2))   = inferTypeHelperPi ctx x t1 t2
inferType ctx (Lambda (Abs x t e)) = inferTypeHelperLambda ctx x t e
inferType ctx (App e1 e2)          = inferTypeHelperApp ctx e1 e2

inferTypeHelperPi :: (MonadInfer m)
                     => Context -> Variable -> Expr -> Expr -> m Expr
inferTypeHelperPi ctx v x y = do a <- inferUniverse ctx x
                                 b <- inferUniverse (extend v x Nothing ctx) y
                                 return $ Universe $ max a b

inferTypeHelperLambda :: (MonadInfer m)
                         => Context -> Variable -> Expr -> Expr -> m Expr
inferTypeHelperLambda ctx v t e = do inferUniverse ctx t
                                     Pi . Abs v t
                                       <$> inferType (extend v t Nothing ctx) e

inferTypeHelperApp :: (MonadInfer m) => Context -> Expr -> Expr -> m Expr
inferTypeHelperApp ctx x y = do Abs v s t <- inferPi ctx x
                                inferType ctx y >>= checkEqual ctx s
                                subst (HM.singleton v y) t

inferUniverse :: (MonadInfer m) => Context -> Expr -> m Word
inferUniverse ctx t = do u <- inferType ctx t
                         n <- normalize ctx u
                         case n of
                           Universe k -> return k
                           _          -> throwTypeExpected

inferPi :: (MonadInfer m) => Context -> Expr -> m Abstraction
inferPi ctx e = do t <- inferType ctx e
                   n <- normalize ctx t
                   case n of
                     Pi ab -> return ab
                     _     -> throwFunctionExpected

checkEqual :: (MonadInfer m) => Context -> Expr -> Expr -> m ()
checkEqual ctx x y = equal ctx x y >>= (`when` throwExpressionsNotEqual x y)

normalize :: (MonadInfer m) => Context -> Expr -> m Expr
normalize _   (Universe k) = return $ Universe k
normalize ctx (Pi ab)      = Pi     <$> normalizeAbstraction ctx ab
normalize ctx (Lambda ab)  = Lambda <$> normalizeAbstraction ctx ab
normalize ctx (Var v)      = normalizeHelperVar v $ ctx ^. at v
normalize ctx (App x y)    = normalizeHelperApp ctx x y

normalizeHelperVar :: (MonadInfer m)
                      => Variable -> Maybe (Expr, Maybe Expr) -> m Expr
normalizeHelperVar v Nothing             = throwUnknownIdentifier v
normalizeHelperVar v (Just (_, Nothing)) = return $ Var v
normalizeHelperVar _ (Just (_, Just x))  = return x

normalizeHelperApp :: (MonadInfer m) => Context -> Expr -> Expr -> m Expr
normalizeHelperApp ctx x y = do
  x' <- normalize ctx x
  y' <- normalize ctx y
  case x' of
    Lambda (Abs v _ e') -> subst (HM.singleton v y) e'
    _                   -> return $ App x' y'

normalizeAbstraction :: (MonadInfer m)
                        => Context -> Abstraction -> m Abstraction
normalizeAbstraction ctx (Abs v t e) = do
  normalize ctx t
  Abs v t <$> normalize (extend v t Nothing ctx) e

equal :: (MonadInfer m) => Context -> Expr -> Expr -> m Bool
equal ctx x y = join (equal' <$> normalize ctx x <*> normalize ctx y)

equal' :: (MonadInfer m) => Expr -> Expr -> m Bool
equal' (Var v1)      (Var v2)      = return $ v1 == v2
equal' (Universe k1) (Universe k2) = return $ k1 == k2
equal' (App x1 y1)   (App x2 y2)   = (&&) <$> equal' x1 x2 <*> equal' y1 y2
equal' (Pi a1)       (Pi a2)       = equalAbstraction a1 a2
equal' (Lambda a1)   (Lambda a2)   = equalAbstraction a1 a2
equal' _             _             = return False

equalAbstraction :: (MonadInfer m) => Abstraction -> Abstraction -> m Bool
equalAbstraction (Abs v1 t1 x) (Abs v2 t2 y)
  = (&&) <$> equal' t1 t2 <*> (equal' x =<< subst (HM.singleton v2 (Var v1)) y)
