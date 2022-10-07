{-# LANGUAGE LambdaCase #-}
module Parser (parse) where

import qualified Data.List as L
import Items
import Types

injectAddPredicate :: Predicate -> Object -> StackEvaluator Object
injectAddPredicate p o = do
  addPredicate o p
  return o

parseObject :: Object -> Fun
parseObject o = Fun (FunT ObjectT ObjectT) ObjectT $ ($ o)

parsePredicate :: Predicate -> Fun
parsePredicate = Fun ObjectT ObjectT . injectAddPredicate

parsePreposition :: (Object -> Predicate -> Predicate) -> Fun
parsePreposition preposition =
  Fun ObjectT (FunT PredicateT PredicateT) $ pure . (pure ... preposition)

parsePredicateModifier :: Predicate -> Fun
parsePredicateModifier predicate =
  Fun (FunT PredicateT PredicateT) (FunT ObjectT ObjectT)
  $ \f -> injectAddPredicate <$> f predicate

-- parsePredicateModifier' :: Predicate -> Fun
-- parsePredicateModifier' predicate =
--   Fun (FunT PredicateT PredicateT) (FunT (FunT PredicateT PredicateT) (FunT ObjectT ObjectT)) $ \f -> do
--   p <- f predicate
--   return $ \g -> injectAddPredicate <$> g p

parseToken :: String -> Fun
parseToken = \case
  "neighbor" -> parseObject Neighbor
  "boat" -> parseObject Boat
  "house" -> parseObject House
  "door" -> parseObject Door

  "a" -> parsePredicate A
  "my" -> parsePredicate My
  "famous" -> parsePredicate Famous

  "on" -> parsePreposition On
  "in" -> parsePreposition In

  "lives" -> parsePredicateModifier Lives
  "eats" -> parsePredicateModifier Eats

  token -> error ("could not parse " ++ token)

parse :: String -> Stack
parse "" = []
parse s =
  let (before, s') = L.span (/= '[') s
      (middle, after) = L.span (/= ']') (L.dropWhile (== '[') s')
      a = map SingleFun (map parseToken (words before))
      b = parse middle
      c = parse (L.dropWhile (== ']') after)
  in a
     ++ (if L.null b then [] else [MultiFun b])
     ++ c
