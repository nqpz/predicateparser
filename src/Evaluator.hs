{-# LANGUAGE GADTs #-}
module Evaluator (evalStack) where

import Types
import Control.Monad (foldM_)
import Data.Maybe (fromMaybe)

evalStackElement :: StackElement -> StackElement -> Maybe (StackEvaluator StackElement)
evalStackElement a b = case (a, b) of
  -- The following cases take care of linking the different parts together.
  (Base t1 thing, Base (TFun t2 resultType) f) ->
    case baseCast t1 t2 of
      Just cast -> Just (Base resultType <$> f (cast thing))
      Nothing -> Nothing

  -- EndToken cases
  (EndToken, Base (TFun TObject resultType) f) ->
    Just (Base resultType <$> f NoObject)

  (EndToken, Base (TFun TPredicate resultType) f) ->
    Just (Base resultType <$> f NoPredicate)

  (EndToken, Base (TFun (TFun TPredicate TPredicate) resultType) f) ->
    Just (Base resultType <$> f (pure . id))

  _ -> Nothing

evalStackElement' :: StackElement -> StackElement -> StackEvaluator StackElement
evalStackElement' a b =
  fromMaybe (error ("undefined combination: (" ++ show a ++ ", " ++ show b ++ ")"))
  $ evalStackElement a b

evalStack :: Stack -> EvalResult
evalStack = execEval . foldM_ evalStackElement' EndToken . reverse
