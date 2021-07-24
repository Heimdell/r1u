
module Fix where

import Data.Function

newtype Fix f = Fix { unFix :: f (Fix f) }

instance
  ( forall a. Eq a => Eq (f a)
  )
  => Eq (Fix f)
  where
    (==) = (==) `on` unFix

instance
  ( forall a. (Ord a, Eq a) => Ord (f a)
  , forall a.        (Eq a) => Eq  (f a)
  )
  => Ord (Fix f)
  where
    compare = compare `on` unFix

instance
  ( forall a. Show a => Show (f a)
  )
  => Show (Fix f)
  where
    show = show . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = go
  where
    go = Fix . fmap go . f

anaM :: (Traversable f, Monad m) => (a -> m (f a)) -> a -> m (Fix f)
anaM f = go
  where
    go = fmap Fix . (traverse go =<<) . f

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = go
  where
    go = f . fmap go . unFix

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM f = go
  where
    go = (f =<<) . traverse go . unFix