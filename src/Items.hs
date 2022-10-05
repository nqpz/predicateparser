module Items ( Object(..)
             , Predicate(..)
             ) where

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
  deriving (Eq, Show)
