{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Evaluator (evalStack) where

import Items
import Types
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

-- compose :: StackElement -> StackElement -> Maybe StackElement
-- compose a b = case (a, b) of
--   (Base (TFun t1 r1) f1, Base (TFun t2 r2) f2) -> do
--     cast <- baseCast r1 t2
--     return $ Base (TFun t1 r2) (\prev -> f1 prev >>= pure . cast >>= f2)
--   _ -> Nothing

compose :: Fun -> Fun -> Maybe Fun
compose (Fun t2 r2 f2) (Fun t1 r1 f1) = do
  cast <- castT r1 t2
  return $ Fun t1 r2 (\prev -> f1 prev >>= pure . cast >>= f2)

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
  _ ->
    undefined

evalStack :: Stack -> EvalResult
evalStack = evalFun . fromMaybe (error "could not compose") . composeStack

-- evalStackElement :: StackElement -> StackElement -> Maybe (StackEvaluator StackElement)
-- evalStackElement a b = case (a, b) of
--   -- The following cases take care of linking the different parts together.
--   (Base t1 thing, Base (TFun t2 resultType) f) ->
--     case baseCast t1 t2 of
--       Just cast -> Just $ Base resultType <$> f (cast thing)
--       Nothing -> Nothing

--   -- EndToken cases
--   (EndToken, Base (TFun TObject resultType) f) ->
--     Just $ Base resultType <$> f NoObject

--   (EndToken, Base (TFun TPredicate resultType) f) ->
--     Just $ Base resultType <$> f NoPredicate

--   (EndToken, Base (TFun (TFun TPredicate TPredicate) resultType) f) ->
--     Just $ Base resultType <$> f (pure . id)

--   _ -> Nothing

-- evalStackElement' :: StackElement -> StackElement -> StackEvaluator StackElement
-- evalStackElement' a b =
--   fromMaybe (error ("undefined combination: (" ++ show a ++ ", " ++ show b ++ ")"))
--   $ evalStackElement a b

-- evalStack :: Stack -> EvalResult
-- evalStack = execEval . foldM_ evalStackElement' EndToken . reverse
