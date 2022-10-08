{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Evaluator (evalStack) where

import Types
import Control.Monad (void)
import Data.Maybe (mapMaybe)

compose :: forall m. Monad m => Funs m -> Funs m -> Funs m
compose f2s f1s =
  let combs = do
        u <- f2s
        t <- f1s
        return (u, t)
      both = mapMaybe check combs ++ mapMaybe checkUnit combs
  in if null both
     then error ("no composition possible:\n" ++ show f2s ++ "\n" ++ show f1s ++ "\n\n")
     else both
  where check :: (Fun m, Fun m) -> Maybe (Fun m)
        check (Fun t2 r2 f2, Fun t1 r1 f1) = do
          cast <- castT r1 t2
          return $ Fun t1 r2 $ \prev -> return prev >>= f1 >>= return . cast >>= f2

        checkUnit :: (Fun m, Fun m) -> Maybe (Fun m)
        checkUnit (Fun UnitT (FunT t2 r2) f2, fun1) = check (Fun t2 r2 (\x -> f2 () >>= ($ x)), fun1)
        checkUnit _ = Nothing

composeFunsGroups :: forall m. Monad m => [FunsGroup m] -> Funs m
composeFunsGroups = foldr1 compose . map collapse
  where collapse :: FunsGroup m -> Funs m
        collapse (SingleFuns f) = f
        collapse (MultiFuns fs) = composeFunsGroups fs

evalFun :: forall f. Applicative f => Funs f -> f ()
evalFun funs = case mapMaybe eval' funs of
  [] -> error "no functions"
  ress -> head ress
  where eval' :: Fun f -> Maybe (f ())
        eval' = \case
          Fun UnitT _ f ->
            ret $ f ()
          Fun (FunT ObjectT ObjectT) _ f ->
            ret $ f (pure . id)
          Fun (FunT PredicateT PredicateT) _ f ->
            ret $ f (pure . id)
          Fun {} ->
            Nothing
        ret :: f a -> Maybe (f ())
        ret = Just . void

evalStack :: Stack -> EvalResult
evalStack = execStackEvaluator . evalFun . composeFunsGroups
