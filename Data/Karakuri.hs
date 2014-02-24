{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Data.Karakuri (
    Karakuri(..)
    , step
    , extract
    , surface
    , iterateM
    ) where

import Control.Monad
import Control.Comonad
import Data.Profunctor

-- | 'Karakuri' means automata in Japanese.
-- It has three type parameters:
-- * Underlying monad /m/
-- * Exterior value /b/ for pushing
-- * Public state /a/
-- This structure aims to encapsulate states while reserving accessibility from the outside.
data Karakuri m b a = Karakuri a (b -> Karakuri m b a) (m (Karakuri m b a))

step :: Karakuri m b a -> m (Karakuri m b a)
step (Karakuri _ _ m) = m
{-# INLINE step #-}

-- | Lens-like interface to a 'Karakuri'. Note that it isn't necessarily a valid lens.
surface :: Functor f => (a -> f b) -> Karakuri m b a -> f (Karakuri m b a)
surface f (Karakuri a bk _) = fmap bk (f a)
{-# INLINE surface #-}

instance Monad m => Functor (Karakuri m b) where
    fmap = rmap
    {-# INLINE fmap #-}

instance Monad m => Applicative (Karakuri m b) where
    pure a = let k = Karakuri a (const k) (return k) in k
    {-# INLINE pure #-}
    Karakuri f bj mj <*> Karakuri a bk mk = Karakuri (f a) (liftA2 (<*>) bj bk) (liftM2 (<*>) mj mk)

instance Monad m => Comonad (Karakuri m b) where
    extract (Karakuri a _ _) = a
    {-# INLINE extract #-}
    extend f k@(Karakuri a bk mk) = Karakuri (f k) (extend f . bk) (extend f `liftM` mk)

instance Monad m => ComonadApply (Karakuri m b) where
    (<@>) = (<*>)
    {-# INLINE (<@>) #-}

instance Monad m => Profunctor (Karakuri m) where
    rmap (Karakuri a bk mk) = Karakuri (f a) (rmap f . bk) (rmap f `liftM` mk)
    lmap cb (Karakuri a bk mk) = Karakuri a (lmap cb . bk . cb) (lmap cb `liftM` mk)

iterateM :: Monad m => (a -> m a) -> a -> Karakuri m a a
iterateM f = let k a = Karakuri a k (k `liftM` f a) in k
{-# INLINE iterateM #-}
