module PredicateParser
  ( module Items
  , module Types
  , module Parser
  , module Evaluator
  , run) where

import Items
import Types
import Parser
import Evaluator

run :: String -> EvalResult
run = evalStack . parse
