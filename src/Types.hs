{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Types ( Object(..)
             , Predicate(..)
             , EvalResult(..)
             , formatEvalResult
             , StackEvaluator
             , addPredicate
             , BaseType(..)
             , baseCast
             , StackElement(..)
             , Stack
             , execEval
             , (...)
             ) where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad.Writer

data Object = Neighbor
            | Boat
            | NoObject
  deriving (Eq, Ord, Show)

data Predicate = My
               | Famous
               | A
               | On Object Predicate
               | Lives
               | NoPredicate
  deriving (Show)

newtype EvalResult = EvalResult { fromEvalResult :: M.Map Object [Predicate] }
  deriving (Show)

instance Semigroup EvalResult where
  a <> b = EvalResult $ M.unionWith (++) (fromEvalResult a) (fromEvalResult b)

instance Monoid EvalResult where
  mempty = EvalResult M.empty

formatEvalResult :: EvalResult -> String
formatEvalResult =
  (++ "\n") . L.intercalate "\n"
  . map (\(o, ps) -> show o ++ ": " ++ L.intercalate ", " (map show ps))
  . M.toList . fromEvalResult

type StackEvaluator = Writer EvalResult

addPredicate :: Object -> Predicate -> StackEvaluator ()
addPredicate _ NoPredicate = return ()
addPredicate NoObject _ = return ()
addPredicate o p = tell (EvalResult $ M.fromList [(o, [p])])

data BaseType a where
  TObject :: BaseType Object
  TPredicate :: BaseType Predicate
  TFun :: BaseType a -> BaseType b -> BaseType (a -> StackEvaluator b)

instance Show (BaseType a) where
  show TObject = "TObject"
  show TPredicate = "TPredicate"
  show (TFun _ _) = "TFun"

data StackElement = forall t. Base (BaseType t) t
                  | EndToken

baseCast :: BaseType a -> BaseType b -> Maybe (a -> b)
baseCast x y = case (x, y) of
  (TObject, TObject) -> Just id
  (TPredicate, TPredicate) -> Just id
  (TFun x1 y1, TFun x2 y2) -> do
    castX <- baseCast x2 x1
    castY <- baseCast y1 y2
    return (\f -> \e -> castY <$> f (castX e))
  _ -> Nothing

instance Show StackElement where
  show (Base t _) = "Base" ++ show t
  show EndToken = "EndToken"

type Stack = [StackElement]

execEval :: Writer EvalResult () -> EvalResult
execEval = execWriter

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x = f . g x
