
module Variable where

class Ord v => Variable v where
  gensym :: Int -> v