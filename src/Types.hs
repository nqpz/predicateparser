{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types ( Object(..)
             , Predicate(..)
             , EvalResult(..)
             , formatEvalResult
             , StackEvaluator
             , addPredicate
             , StackElement(..)
             , Stack
             , execEval
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

data StackElement = Object Object
                  | Predicate Predicate
                  | FunctionOO (Object -> StackEvaluator Object)
                  | FunctionPO (Predicate -> StackEvaluator Object)
                  | FunctionPP_P ((Predicate -> Predicate) -> StackEvaluator Predicate)
                  | FunctionO_PP (Object -> StackEvaluator (Predicate -> Predicate))
                  | FunctionPP (Predicate -> Predicate)
                  | EndToken

instance Show StackElement where
  show (Object o) = "Object" ++ show o
  show (Predicate p) = "Predicate" ++ show p
  show (FunctionOO _) = "FunctionOO"
  show (FunctionPO _) = "FunctionPO"
  show (FunctionPP_P _) = "FunctionPP_P"
  show (FunctionO_PP _) = "FunctionO_PP"
  show (FunctionPP _) = "FunctionPP"
  show EndToken = "EndToken"

type Stack = [StackElement]

execEval :: Writer EvalResult () -> EvalResult
execEval = execWriter
