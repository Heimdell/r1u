
module BindingMonad where

import Control.Arrow (first, second)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Foldable as Foldable (toList, for_, fold)
import Data.Monoid
import Data.Functor ((<&>))
import Data.Function ((&))
-- import Data.Traversable (for)

import Free
import Unifiable
import Variable

class
  ( Unifiable t
  , Variable v
  , MonadError (TypeError t v) m
  )
  => BindingMonad t v m | m v -> t, m t -> v
  where
    find :: v -> m (Maybe (Free t v))
    free :: m v
    (=:) :: v -> Free t v -> m (Free t v)

newVar :: BindingMonad t v m => Free t v -> m v
newVar t = do
  v <- free
  v =: t
  return v

semiprune :: BindingMonad t v m => Free t v -> m (Free t v)
semiprune it = case it of
  Pure a -> loop it a
  Free _ -> return it
  where
    loop t v = do
      find v >>= \case
        Just (Pure v') -> do
          vn <- loop t v'
          v =: vn
          return vn
        _ -> return t

fullprune :: BindingMonad t v m => Free t v -> m (Free t v)
fullprune it = case it of
  Free _ -> return it
  Pure v -> do
    find v >>= \case
      Nothing -> return it
      Just t -> do
        tn <- fullprune t
        v =: tn
        return tn

allFreeVars :: ∀ t v m f. (BindingMonad t v m, Foldable f) => f (Free t v) -> m [v]
allFreeVars terms
  = for_ (toList terms) loop
  & flip execStateT Set.empty
  & fmap toList
  where
    loop :: Free t v -> StateT (Set.Set v) m ()
    loop term = do
      semiprune term >>= \case
        Free t -> for_ t loop
        Pure v -> do
          seen <- gets (Set.member v)
          unless seen do
            modify $ Set.insert v
            t <- find v
            for_ t loop

occursIn :: BindingMonad t v m => v -> Free t v -> m Bool
occursIn v t = fullprune t <&> (getAny . Free.fold Foldable.fold (Any . (== v)))

data TypeError t v
  = Occurs v (Free t v)
  | Mismatch (t (Free t v)) (t (Free t v))

(=!) :: (BindingMonad t v m) => v -> t (Free t v) -> StateT (Map.Map v (t (Free t v))) m ()
v =! t = do
  gets (Map.lookup v) >>= \case
    Nothing -> modify $ Map.insert v t
    Just t' -> lift $ throwError $ Occurs v (Free t')

applyAllBindings
  :: ( BindingMonad t v m
     , Traversable f
     )
  =>    f (Free t v)
  -> m (f (Free t v))

applyAllBindings terms = traverse applyBindings terms `evalStateT` Map.empty
  where
    applyBindings term = do
      pruned <- semiprune term
      case pruned of
        Free shell -> Free <$> traverse applyBindings shell
        Pure var -> do
          gets (Map.lookup var) >>= \case
            Just (Right done)      -> return done
            Just (Left  blackhole) -> throwError $ Occurs var blackhole
            Nothing -> do
              find var >>= \case
                Nothing -> return pruned
                Just inner  -> do
                  modify $ Map.insert var $ Left inner
                  done <- applyBindings inner
                  modify $ Map.insert var $ Right done
                  return done

freshenAll
  :: ∀ t v m f
  .  ( BindingMonad t v m
     , Traversable f
     )
  =>    f (Free t v)
  -> m (f (Free t v))

freshenAll terms = traverse freshen terms `evalStateT` Map.empty
  where
    freshen term = do
      semiprune term >>= \case
        Free shell -> Free <$> traverse freshen shell
        Pure var   -> do
          gets (Map.lookup var) >>= \case
            Just (Right done) -> return done
            Just (Left  blackhole) -> throwError $ Occurs var blackhole
            Nothing -> do
              find var >>= \case
                Nothing -> do
                  new <- Pure <$> free
                  modify $ Map.insert var $ Right new
                  return new

                Just shell -> do
                  modify $ Map.insert var $ Left shell
                  done <- freshen shell
                  res  <- Pure <$> newVar done
                  modify $ Map.insert var $ Right res
                  return res

(=:=) :: (BindingMonad t v m) => Free t v -> Free t v -> m (Free t v)
l' =:= r' = do
  (l' =?= r') `evalStateT` Map.empty
  where
    l =?= r = do
      lPruned <- semiprune l
      rPruned <- semiprune r
      case (lPruned, rPruned) of
        (Pure lVar, Pure rVar)
          | lVar == rVar -> return rPruned
          | otherwise -> do
            ml <- find lVar
            mr <- find rVar
            case (ml, mr) of
              (Nothing, Nothing) -> lVar =: rPruned
              (Nothing, Just _)  -> lVar =: rPruned
              (Just _,  Nothing) -> rVar =: lPruned

              (Just (Free lShell), Just (Free rShell)) -> do
                term <- rollback do
                  lVar =! lShell
                  rVar =! rShell
                  match lShell rShell
                _ <- rVar =: term
                lVar =: rPruned

              _ -> error "(=:=): internal error"

        (Pure lVar, Free rShell) -> do
          find lVar >>= \case
            Nothing -> do
              return rPruned

            Just (Free lShell) -> do
              res <- rollback do
                lVar =! lShell
                match lShell rShell

              lVar =: res
              return lPruned

            _ -> error "(=:=): internal error"

        (Free lShell, Pure rVar) -> do
          find rVar >>= \case
            Nothing -> do
              return lPruned

            Just (Free rShell) -> do
              res <- rollback do
                rVar =! rShell
                match lShell rShell

              rVar =: res
              return rPruned

            _ -> error "(=:=): internal error"

        (Free lShell, Free rShell) -> do
          match lShell rShell

    match l r = do
      case zipMatch l r of
        Nothing -> throwError $ Mismatch l r
        Just inifier -> Free <$> traverse (either return (uncurry (=?=))) inifier

type BindingState t v = (Int, Map.Map v (Free t v))
type BindingT     t v = StateT (BindingState t v)

rollback :: (MonadState s m) => m a -> m a
rollback action = do
  s <- get
  r <- action
  put s
  return r

instance
  ( Unifiable t
  , Variable v
  , MonadError (TypeError t v) m
  )
  => BindingMonad t v (BindingT t v m)
  where
    find v = gets (Map.lookup v . snd)
    free = do
      modify $ first (+ 1)
      gets (gensym . fst)

    v =: f = do
      modify $ second $ Map.insert v f
      return f

instance {-# overlappable #-}
  ( BindingMonad t v m
  , MonadTrans n
  , MonadError (TypeError t v) (n m)
  )
  => BindingMonad t v (n m)
  where
    find = lift . find
    free = lift   free
    (=:) = (lift .) . (=:)

type InferenceT t v m = BindingT t v (ExceptT (TypeError t v) m)
type Inference  t v   = InferenceT t v Identity

runInferenceT :: Monad m => InferenceT t v m a -> m (Either (TypeError t v) a)
runInferenceT action =
  action
  & flip evalStateT (0, Map.empty)
  & runExceptT

runInference :: Inference t v a -> Either (TypeError t v) a
runInference = runIdentity . runInferenceT