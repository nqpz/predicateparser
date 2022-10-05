{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Evaluator (evalStack) where

import Items
import Types
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

compose :: Fun -> Fun -> Maybe Fun
compose (Fun t2 r2 f2) (Fun t1 r1 f1) = do
  cast <- castT r1 t2
  return $ Fun t1 r2 $ \prev -> pure prev >>= f1 >>= pure . cast >>= f2

composeStack :: Stack -> Maybe Fun
composeStack funs = do
  (first, rest) <- case funs of
    first : rest -> Just (first, rest)
    _ -> Nothing
  foldM compose first rest

evalFun :: Fun -> EvalResult
evalFun = \case
  Fun ObjectT _ f ->
    execEval $ f NoObject
  Fun PredicateT _ f ->
    execEval $ f NoPredicate
  Fun (FunT PredicateT PredicateT) _ f ->
    execEval $ f (pure . id)
  fun@(Fun {}) ->
    error ("could not handle function " ++ show fun)

evalStack :: Stack -> EvalResult
evalStack = evalFun . fromMaybe (error "could not compose stack") . composeStack
