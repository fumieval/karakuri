{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Data.Karakuri (
    Karakuri(..)
    , Karakuri'
    , step
    , transKarakuri
    , stateful
    , stateful'
    , effective
    ) where

import Control.Monad.Trans.State
import Control.Applicative
import Control.Comonad
import Data.Functor.Identity
import Control.Monad

-- | Karakuri means automaton in Japanese.
data Karakuri m a = forall s. Karakuri (s -> m s) (s -> a) s

-- | Run a 'Karakuri'.
step :: Monad m => Karakuri m a -> m (Karakuri m a)
step (Karakuri m f s) = Karakuri m f `liftM` m s

instance Functor (Karakuri m) where
    fmap f (Karakuri m g s) = Karakuri m (f . g) s
    {-# INLINE fmap #-}

instance Monad m => Applicative (Karakuri m) where
    pure a = Karakuri return (const a) ()
    {-# INLINE pure #-}
    Karakuri m f s <*> Karakuri n g t = Karakuri
        (\(a, b) -> m a >>= \r -> n b >>= \s -> return (r, s))
        (\(x, y) -> f x (g y))
        (s, t)

instance Comonad (Karakuri m) where
    extract (Karakuri _ f s) = f s
    {-# INLINE extract #-}
    extend k (Karakuri m f s) = Karakuri m (k . Karakuri m f) s
    {-# INLINE extend #-}

instance Monad m => ComonadApply (Karakuri m) where
    (<@>) = (<*>)
    {-# INLINE (<@>) #-}

transKarakuri :: (forall s. m s -> n s) -> Karakuri m a -> Karakuri n a
transKarakuri t (Karakuri f e s) = Karakuri (t . f) e s

-- | Create a 'Karakuri' from the stateful action.
stateful :: Monad m => StateT s m () -> s -> Karakuri m s
stateful m s = Karakuri (execStateT m) id s

type Karakuri' = Karakuri Identity

-- | Create a 'Karakuri' from the stateful action.
stateful' :: Monad m => State s () -> s -> Karakuri m s
stateful' m s = Karakuri (return . execState m) id s

-- | Create a 'Karakuri' that performs the given action every time.
effective :: Monad m => a -> m a -> Karakuri m a
effective a m = Karakuri (const m) id a
