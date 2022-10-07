{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Types ( EvalResult(..)
             , formatEvalResult
             , StackEvaluator
             , addPredicate
             , Stack
             , T(..)
             , castT
             , Fun(..)
             , FunGroup(..)
             , execEval
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

execEval :: StackEvaluator a -> EvalResult
execEval = execWriter

formatEvalResult :: EvalResult -> String
formatEvalResult =
  (++ "\n") . L.intercalate "\n"
  . map (\(o, ps) -> show o ++ ": " ++ L.intercalate ", " (map show ps))
  . M.toList . fromEvalResult

addPredicate :: Object -> Predicate -> StackEvaluator ()
addPredicate _ NoPredicate = pure ()
addPredicate NoObject _ = pure ()
addPredicate o p = tell $ EvalResult $ M.singleton o [p]

data T a where
  ObjectT :: T Object
  PredicateT :: T Predicate
  FunT :: T a -> T b -> T (a -> StackEvaluator b)

instance Show (T a) where
  show ObjectT = "ObjectT"
  show PredicateT = "PredicateT"
  show (FunT t u) = "FunT (" ++ show t ++ ") (" ++ show u ++ ") <function>"

data Fun = forall a b. Fun (T a) (T b) (a -> StackEvaluator b)

instance Show Fun where
  show (Fun t u _) = "Fun (" ++ show t ++ ") (" ++ show u ++ ") <function>"

data FunGroup = SingleFun Fun
              | MultiFun [FunGroup]
  deriving (Show)

type Stack = [FunGroup]

castT :: T a -> T b -> Maybe (a -> b)
castT x y = case (x, y) of
  (ObjectT, ObjectT) -> Just id
  (PredicateT, PredicateT) -> Just id
  (FunT x1 y1, FunT x2 y2) -> do
    castX <- castT x2 x1
    castY <- castT y1 y2
    pure $ \f e -> castY <$> f (castX e)
  _ -> Nothing

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x = f . g x
