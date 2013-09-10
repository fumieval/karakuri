{-# LANGUAGE ExistentialQuantification, GADTs, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts #-}

module Data.Karakuri where

import Control.Monad.Trans.State
import Control.Applicative
import Control.Comonad
import Data.Functor.Identity
import Control.Monad
import Data.IntMap.Strict as IM
import Control.Monad.Trans.Operational.Mini
import Control.Monad.Operational.TH
import Unsafe.Coerce

data Karakuri m a = forall s. Karakuri (s -> m s) (s -> a) s

step :: Monad m => Karakuri m a -> m (Karakuri m a)
step (Karakuri m f s) = do
    s' <- m s
    return $ Karakuri m f s'

instance Functor (Karakuri m) where
    fmap f (Karakuri m g s) = Karakuri m (f . g) s
    {-# INLINE fmap #-}

instance Monad m => Applicative (Karakuri m) where
    pure a = Karakuri return (const a) ()
    {-# INLINE pure #-}
    Karakuri m f s <*> Karakuri n g t = Karakuri (\(a, b) -> m a >>= \r -> n b >>= \s -> return (r, s)) (\(x, y) -> f x (g y)) (s, t)

instance Comonad (Karakuri m) where
    extract (Karakuri _ f s) = f s
    {-# INLINE extract #-}
    extend k (Karakuri m f s) = Karakuri m (k . Karakuri m f) s
    {-# INLINE extend #-}

stateful :: Monad m => StateT s m () -> s -> Karakuri m s
stateful m s = Karakuri (execStateT m) id s

type Karakuri' = Karakuri Identity

stateful' :: Monad m => State s () -> s -> Karakuri m s
stateful' m s = Karakuri (return . execState m) id s

effective :: Monad m => m a -> Karakuri m (Maybe a)
effective m = Karakuri (const $ Just `liftM` m) id Nothing

newtype Kao a = Kao Int

data ButaiBase k a where
    Register :: k r -> ButaiBase k (Kao r)
    Look :: Kao r -> ButaiBase k r
    Kill :: Kao r -> ButaiBase k ()
    Tick :: ButaiBase k ()
makeSingletons ''ButaiBase

newtype ButaiT m a = ButaiT { unButaiT :: ReifiedProgramT (ButaiBase (Karakuri (ButaiT m))) m a } deriving (Monad, Applicative, Functor)

runButaiT :: Monad m => ButaiT m a -> m a
runButaiT = go 0 IM.empty . unButaiT where
    go i m (Register k :>>= cont) = go (succ i) (IM.insert i (unsafeCoerce k) m) $ cont (Kao i)
    go i m (Look k@(Kao j) :>>= cont) = go i m $ cont $ extract (unsafeCoerce (m IM.! j) `asKarakuriOf` k)
    go i m (Kill (Kao j) :>>= cont) = go i (IM.delete j m) $ cont ()
    go i m (Tick :>>= cont) = do
        rs <- forM (IM.toAscList m) (\(i, m) -> (,) i `liftM` step m)
        go i (IM.fromAscList rs) (cont ())

    asKarakuriOf :: Karakuri m a -> p a -> Karakuri m a
    asKarakuriOf x _ = x
