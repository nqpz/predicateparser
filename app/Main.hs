module Main (main) where

import PredicateParser

main :: IO ()
main = interact (formatEvalResult . run)
