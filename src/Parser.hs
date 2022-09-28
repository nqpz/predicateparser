module Parser (parse) where

import Types

parsePredicate :: Predicate -> StackElement
parsePredicate p = Function TObject TObject $ \o -> do
  addPredicate o p
  return o

parseObject :: Object -> StackElement
parseObject o = Function TPredicate TObject $ \p -> do
  addPredicate o p
  return o

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x = f . g x

parseToken :: String -> StackElement
parseToken token = case token of
  "my" -> parsePredicate My
  "famous" -> parsePredicate Famous
  "neighbor" -> parseObject Neighbor
  "lives" -> Function (TFun TPredicate TPredicate) TPredicate ($ Lives)
  "on" -> Function TObject (TFun TPredicate TPredicate) $ return . (return ... On)
  "a" -> parsePredicate A
  "boat" -> parseObject Boat
  _ -> error ("could not parse " ++ token)

parse :: String -> [StackElement]
parse = map parseToken . words
