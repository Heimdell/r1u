
module Free where

import Control.Monad (join)

import Fix

data Free f a
  = Free (f (Free f a))
  | Pure a

foldM :: (Monad m, Traversable f) => (f b -> m b) -> (a -> m b) -> Free f a -> m b
foldM unfree unpure = go
  where
    go = \case
      Free f -> unfree =<< traverse go f
      Pure a -> unpure a

fold :: (Functor f) => (f c -> c) -> (a -> c) -> Free f a -> c
fold unfree unpure = go
  where
    go = \case
      Free f -> unfree $ fmap go f
      Pure a -> unpure a

retract :: Monad m => Free m a -> m a
retract = fold join return

wrap :: Functor f => f a -> Free f a
wrap = Free . fmap Pure

instance (Eq (f (Free f a)), Eq a) => Eq (Free f a) where
  Pure a == Pure b = a == b
  Free f == Free g = f == g
  _      == _      = False

instance (Ord (f (Free f a)), Ord a) => Ord (Free f a) where
  Pure a <= Pure b = a <= b
  Pure _ <= Free _ = True
  Free f <= Free g = f <= g
  Free _ <= Pure _ = False

newtype Unshow = Unshow { doShow :: String }

instance Show Unshow where
  show = doShow

unshow :: Show a => a -> Unshow
unshow = Unshow . show

instance (Show (f Unshow), Show a, Functor f) => Show (Free f a) where
  show = doShow . fold unshow unshow

freeze :: Traversable f => Free f a -> Maybe (Fix f)
freeze = foldM (return . Fix) (\_ -> Nothing)

thaw :: Functor f => Fix f -> Free f a
thaw = cata Free