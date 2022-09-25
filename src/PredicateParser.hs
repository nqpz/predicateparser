module PredicateParser
  ( module Types
  , module Parser
  , module Evaluator
  , run) where

import Types
import Parser
import Evaluator

run :: String -> EvalResult
run = evalStack . parse
