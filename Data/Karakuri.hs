{-# LANGUAGE ExistentialQuantification #-}

module Data.Karakuri where

import Control.Monad.Trans.State
import Control.Applicative
import Control.Comonad
import Data.Functor.Identity
import Control.Monad

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
