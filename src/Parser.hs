module Parser (parse) where

import Types

parsePredicate :: Predicate -> StackElement
parsePredicate p = FunctionOO $ \o -> do
  addPredicate o p
  return o

parseObject :: Object -> StackElement
parseObject o = FunctionPO $ \p -> do
  addPredicate o p
  return o

parseToken :: String -> StackElement
parseToken token = case token of
  "my" -> parsePredicate My
  "famous" -> parsePredicate Famous
  "neighbor" -> parseObject Neighbor
  "lives" -> FunctionPP_P $ \f -> return $ f Lives
  "on" -> FunctionO_PP $ \o -> return $ \p -> On o p
  "a" -> parsePredicate A
  "boat" -> parseObject Boat
  _ -> error ("could not parse " ++ token)

parse :: String -> [StackElement]
parse = map parseToken . words
