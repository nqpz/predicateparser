module Items ( Object(..)
             , Predicate(..)
             ) where

data Object = NoObject
            | Neighbor
            | Boat
            | House
            | Door
  deriving (Eq, Ord, Show)

data Predicate = NoPredicate
               | A
               | My
               | Famous
               | Lives
               | Eats
               | On Object Predicate
               | In Object Predicate
  deriving (Eq, Show)
