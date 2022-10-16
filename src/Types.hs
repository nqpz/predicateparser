{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Types ( EvalResult(..)
             , formatEvalResult
             , StackEvaluator
             , execStackEvaluator
             , addPredicate
             , T(..)
             , castT
             , Fun(..)
             , Funs
             , FunsGroup(..)
             , Stack
             , StackFuns
             , (...)
             ) where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad.Writer
import Items

newtype EvalResult = EvalResult { fromEvalResult :: M.Map Object [Predicate] }
  deriving (Show)

instance Semigroup EvalResult where
  a <> b = EvalResult $ M.unionWith (++) (fromEvalResult a) (fromEvalResult b)

instance Monoid EvalResult where
  mempty = EvalResult M.empty

type StackEvaluator = Writer EvalResult

execStackEvaluator :: StackEvaluator () -> EvalResult
execStackEvaluator = execWriter

formatEvalResult :: EvalResult -> String
formatEvalResult =
  (++ "\n") . L.intercalate "\n"
  . map (\(o, ps) -> show o ++ ": " ++ L.intercalate ", " (map show ps))
  . M.toList . fromEvalResult

addPredicate :: Object -> Predicate -> StackEvaluator ()
addPredicate o p = tell $ EvalResult $ M.singleton o [p]

data T f a where
  UnitT :: T f ()
  ObjectT :: T f Object
  PredicateT :: T f Predicate
  FunT :: T f a -> T f b -> T f (a -> f b)

instance Show (T f a) where
  show UnitT = "()"
  show ObjectT = "O"
  show PredicateT = "P"
  show (FunT t u) = "(" ++ show t ++ " -> " ++ show u ++ ")"

data Fun f = forall a b. Fun (T f a) (T f b) (a -> f b)

instance Show (Fun f) where
  show (Fun t u _) = "(" ++ show t ++ " -> " ++ show u ++ ")"

type Funs f = [Fun f]

data FunsGroup f = SingleFuns (Funs f)
                 | MultiFuns [FunsGroup f]
  deriving (Show)

type StackFuns = Funs StackEvaluator

type Stack = [FunsGroup StackEvaluator]

castT :: Functor f => T f a -> T f b -> Maybe (a -> b)
castT x y = case (x, y) of
  (UnitT, UnitT) -> Just id
  (ObjectT, ObjectT) -> Just id
  (PredicateT, PredicateT) -> Just id
  (FunT x1 y1, FunT x2 y2) -> do
    castX <- castT x2 x1
    castY <- castT y1 y2
    pure $ \f e -> castY <$> f (castX e)
  _ -> Nothing

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x = f . g x
