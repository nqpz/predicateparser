{-# LANGUAGE LambdaCase #-}
module Parser (parse) where

import Items
import Types

injectAddPredicate :: Predicate -> Object -> StackEvaluator Object
injectAddPredicate p o = do
  addPredicate o p
  return o

parseObject :: Object -> StackFuns
parseObject o = [ Fun (FunT ObjectT ObjectT) ObjectT $ ($ o)
                , Fun UnitT ObjectT $ \() -> pure o
                ]

parseAdjective :: Predicate -> StackFuns
parseAdjective p = [Fun ObjectT ObjectT $ injectAddPredicate p]

parsePreposition :: (Object -> Predicate -> Predicate) -> StackFuns
parsePreposition preposition = [Fun ObjectT (FunT PredicateT PredicateT) $ pure . (pure ... preposition)]

parseModifier :: Predicate -> StackFuns
parseModifier predicate =
  [ Fun (FunT PredicateT PredicateT) (FunT ObjectT ObjectT)
    $ \f -> injectAddPredicate <$> f predicate

  , Fun (FunT PredicateT PredicateT) (FunT (FunT PredicateT PredicateT) (FunT ObjectT ObjectT)) $ \f -> do
      p <- f predicate
      return $ \g -> injectAddPredicate <$> g p
  ]

parseToken :: String -> StackFuns
parseToken = \case
  "neighbor" -> parseObject Neighbor
  "boat" -> parseObject Boat
  "house" -> parseObject House
  "door" -> parseObject Door

  "a" -> parseAdjective A
  "the" -> parseAdjective The
  "my" -> parseAdjective My
  "famous" -> parseAdjective Famous

  "on" -> parsePreposition On
  "in" -> parsePreposition In

  "lives" -> parseModifier Lives
  "eats" -> parseModifier Eats

  token -> error ("could not parse " ++ token)

parse :: String -> Stack
parse "" = []
parse s =
  let (before, s') =span (/= '[') s
      (middle, after) = span (/= ']') (dropWhile (== '[') s')
      a = map SingleFuns (map parseToken (words before))
      b = parse middle
      c = parse (dropWhile (== ']') after)
  in a
     ++ (if null b then [] else [MultiFuns b])
     ++ c
