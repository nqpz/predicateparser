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

parsePreposition :: (Object -> Predicate -> Predicate) -> Fun
parsePreposition preposition =
  Fun ObjectT (FunT PredicateT PredicateT) $ pure . (pure ... preposition)

parsePredicateModifier :: Predicate -> Fun
parsePredicateModifier predicate =
  Fun (FunT PredicateT PredicateT) PredicateT ($ predicate)

parseToken :: String -> Fun
parseToken token = case token of
  "a" -> parsePredicate A
  "my" -> parsePredicate My
  "famous" -> parsePredicate Famous

  "lives" -> parsePredicateModifier Lives
  "eats" -> parsePredicateModifier Eats

  "on" -> parsePreposition On
  "in" -> parsePreposition In

  "neighbor" -> parseObject Neighbor
  "boat" -> parseObject Boat
  "house" -> parseObject House
  "door" -> parseObject Door
  _ -> error ("could not parse " ++ token)

parse :: String -> Stack
parse = map parseToken . words
