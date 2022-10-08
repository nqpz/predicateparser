{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Evaluator (evalStack) where

import Types
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

trace' :: String -> a -> a
trace' _ x = x

compose :: Funs -> Funs -> Funs
compose f2s f1s =
  let combs = do
        u <- trace' (show f2s) f2s
        t <- trace' (show f1s) f1s
        return (u, t)
      both = mapMaybe check combs ++ mapMaybe checkUnit combs
  in if null both
     then error ("no composition possible:\n" ++ show f2s ++ "\n" ++ show f1s ++ "\n\n")
     else trace' (show both) both
  where check :: (Fun, Fun) -> Maybe Fun
        check (Fun t2 r2 f2, Fun t1 r1 f1) = do
          cast <- castT r1 t2
          return $ Fun t1 r2 $ \prev -> pure prev >>= f1 >>= pure . cast >>= f2
        checkUnit :: (Fun, Fun) -> Maybe Fun
        checkUnit (Fun UnitT (FunT t2 r2) f2, fun1) = check (Fun t2 r2 f2', fun1)
          where f2' x = do
                  g <- f2 ()
                  g x
        checkUnit _ = Nothing

composeStack :: Stack -> Funs
composeStack = foldr1 compose . map collapse
  where collapse :: FunsGroup -> Funs
        collapse (SingleFuns f) = f
        collapse (MultiFuns fs) = composeStack fs

evalFun :: Funs -> EvalResult
evalFun funs = trace' (show funs) $ case mapMaybe eval' funs of
  [] -> error "no functions"
  ress -> head ress
  where eval' :: Fun -> Maybe EvalResult
        eval' = \case
          Fun UnitT _ f ->
            Just $ execEval $ f ()
          Fun (FunT ObjectT ObjectT) _ f ->
            Just $ execEval $ f (pure . id)
          Fun (FunT PredicateT PredicateT) _ f ->
            Just $ execEval $ f (pure . id)
          Fun {} ->
            Nothing
          -- fun@(Fun {}) ->
          --   error ("could not handle function " ++ show fun)

evalStack :: Stack -> EvalResult
evalStack = evalFun . composeStack
