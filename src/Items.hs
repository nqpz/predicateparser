module Items ( Object(..)
             , Predicate(..)
             ) where

data Object = Neighbor
            | Boat
            | House
            | Door
  deriving (Eq, Ord, Show)

data Predicate = A
               | The
               | My
               | Famous
               | Lives
               | Eats
               | On Object Predicate
               | In Object Predicate
  deriving (Eq, Show)
