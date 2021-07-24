
module Variable where

import Data.String

class Ord v => Variable v where
  gensym :: Int -> v

  default gensym :: IsString v => Int -> v
  gensym i = fromString $ "a" ++ show i
