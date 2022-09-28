{-# LANGUAGE GADTs #-}
module Evaluator (evalStack) where

import Types
import Control.Monad

(===) :: BaseType a -> BaseType b -> Bool
x === y = case (x, y) of
  (TObject, TObject) -> True
  (TPredicate, TPredicate) -> True
  (TFun x1 y1, TFun x2 y2) -> x1 === x2 && y1 === y2
  _ -> False

baseCast :: BaseType a -> BaseType b -> (a -> b)
baseCast x y = case (x, y) of
  (TObject, TObject) -> id
  (TPredicate, TPredicate) -> id
  (TFun x1 y1, TFun x2 y2) ->
    \f -> \e -> baseCast y1 y2 <$> f (baseCast x2 x1 e)
  _ -> error "no match"

evalStackElement :: StackElement -> StackElement -> StackEvaluator StackElement
evalStackElement a b = case (a, b) of
  -- EndToken cases
  (EndToken, Base (TFun TObject TObject) f) ->
    Base TObject <$> f NoObject
  (EndToken, Base (TFun TObject (TFun TPredicate TPredicate)) f) ->
    Base (TFun TPredicate TPredicate) <$> f NoObject

  (EndToken, Base (TFun TPredicate TObject) f) ->
    Base TObject <$> f NoPredicate
  (EndToken, Base (TFun TPredicate TPredicate) f) ->
    Base TPredicate <$> f NoPredicate

  (EndToken, Base (TFun (TFun TPredicate TPredicate) TPredicate) f) ->
    Base TPredicate <$> f (return . id)

  (EndToken, other) ->
    error ("unhandled EndToken combination with " ++ show other)
  (_, EndToken) ->
    error "unexpected EndToken at start"

  -- The following cases take care of linking the different parts together.
  (Base t1 thing, Base (TFun t2 resultType) f) | t1 === t2 ->
    Base resultType <$> f (baseCast t1 t2 thing)

  -- (Base TObject o, Base (TFun TObject TObject) f) -> fmap (Base TObject) $ f o
  -- (Base TObject o, Base (TFun TObject (TFun TPredicate TPredicate)) f) -> do
  --   pp <- f o
  --   return $ Base (TFun TPredicate TPredicate) pp
  -- (Base (TFun TPredicate TPredicate) pp, Base (TFun (TFun TPredicate TPredicate) TPredicate) f) -> do
  --   p <- f pp
  --   return $ Base TPredicate p
  -- (Base TPredicate p, Base (TFun TPredicate TObject) f) -> do
  --   o <- f p
  --   return $ Base TObject o
  _ -> error ("undefined combination: (" ++ show a ++ ", " ++ show b ++ ")")

evalStack :: Stack -> EvalResult
evalStack = execEval . foldM_ evalStackElement EndToken . reverse
