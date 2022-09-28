{-# LANGUAGE GADTs #-}
module Evaluator (evalStack) where

import Types
import Control.Monad

evalStackElement :: StackElement -> StackElement -> StackEvaluator StackElement
evalStackElement a b = case (a, b) of
  -- EndToken cases
  (EndToken, Function TObject TObject f) ->
    Base TObject <$> f NoObject
  (EndToken, Function TPredicate TObject f) ->
    Base TObject <$> f NoPredicate
  (EndToken, Function (TFun TPredicate TPredicate) TPredicate f) ->
    Base TPredicate <$> f (return . id)
  (EndToken, Function TObject (TFun TPredicate TPredicate) f) ->
    Function TPredicate TPredicate <$> f NoObject
  (EndToken, Function TPredicate TPredicate f) ->
    Base TPredicate <$> f NoPredicate
  (EndToken, other) ->
    error ("unhandled EndToken combination with " ++ show other)
  (_, EndToken) ->
    error "unexpected EndToken at start"

  -- The following cases take care of linking the different parts together.
  (Base TObject o, Function TObject TObject f) -> fmap (Base TObject) $ f o
  (Base TObject o, Function TObject (TFun TPredicate TPredicate) f) -> do
    pp <- f o
    return $ Function TPredicate TPredicate pp
  (Function TPredicate TPredicate pp, Function (TFun TPredicate TPredicate) TPredicate f) -> do
    p <- f pp
    return $ Base TPredicate p
  (Base TPredicate p, Function TPredicate TObject f) -> do
    o <- f p
    return $ Base TObject o
  _ -> error ("undefined combination: (" ++ show a ++ ", " ++ show b ++ ")")

evalStack :: Stack -> EvalResult
evalStack = execEval . foldM_ evalStackElement EndToken . reverse
