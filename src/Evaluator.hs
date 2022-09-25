module Evaluator (evalStack) where

import Types
import Control.Monad

evalStackElement :: StackElement -> StackElement -> StackEvaluator StackElement
evalStackElement a b = case (a, b) of
  (EndToken, FunctionOO f) -> do
    o <- f NoObject
    return $ Object o
  (EndToken, FunctionPO f) -> do
    o <- f NoPredicate
    return $ Object o
  -- Probably we only need the two cases above.
  (EndToken, FunctionPP_P f) -> do
    p <- f id
    return $ Predicate p
  (EndToken, FunctionO_PP f) -> do
    pp <- f NoObject
    return $ FunctionPP pp
  (EndToken, FunctionPP f) ->
    return $ Predicate $ f NoPredicate
  -- The following cases take care of linking the different parts together.
  (Object o, FunctionOO f) -> fmap Object $ f o
  (Object o, FunctionO_PP f) -> fmap FunctionPP $ f o
  (FunctionPP pp, FunctionPP_P f) -> fmap Predicate $ f pp
  (Predicate p, FunctionPO f) -> fmap Object $ f p
  _ -> error ("undefined combination: (" ++ show a ++ ", " ++ show b ++ ")")

evalStack :: Stack -> EvalResult
evalStack = execEval . foldM_ evalStackElement EndToken . reverse
