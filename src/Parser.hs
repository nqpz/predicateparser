module Parser (parse) where

import Items
import Types

parsePredicate :: Predicate -> Fun
parsePredicate p = Fun ObjectT ObjectT $ \o -> do
  addPredicate o p
  return o

parseObject :: Object -> Fun
parseObject o = Fun PredicateT ObjectT $ \p -> do
  addPredicate o p
  return o

parseToken :: String -> Fun
parseToken token = case token of
  "my" -> parsePredicate My
  "famous" -> parsePredicate Famous
  "neighbor" -> parseObject Neighbor
  "lives" -> Fun (FunT PredicateT PredicateT) PredicateT ($ Lives)
  "on" -> Fun ObjectT (FunT PredicateT PredicateT) $ return . (return ... On)
  "a" -> parsePredicate A
  "boat" -> parseObject Boat
  _ -> error ("could not parse " ++ token)

parse :: String -> Stack
parse = map parseToken . words
