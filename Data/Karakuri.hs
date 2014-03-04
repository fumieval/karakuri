{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Data.Karakuri (
    Karakuri(..)
    , transKarakuri
    , extract
    , surface
    , iterateM
    ) where

import Control.Monad
import Control.Comonad
import Data.Profunctor
import Control.Applicative
import Data.Distributive

-- | 'Karakuri' means automata in Japanese.
-- It has three type parameters:
-- * Underlying monad /m/
-- * Exterior value /b/ for pushing
-- * Public state /a/
-- This structure aims to encapsulate states while reserving accessibility from the outside.
data Karakuri m a b = Karakuri
    { look :: b
    , feed :: a -> Karakuri m a b
    , step :: m (Karakuri m a b)
    }

transKarakuri :: Monad n => (forall x. m x -> n x) -> Karakuri m a b -> Karakuri n a b
transKarakuri t = go where go (Karakuri a bk mk) = Karakuri a (go . bk) (go `liftM` t mk)
{-# INLINE transKarakuri #-}

-- | Lens-like interface to a 'Karakuri'. Note that it isn't necessarily a valid lens.
surface :: Functor f => (a -> f b) -> Karakuri m b a -> f (Karakuri m b a)
surface f (Karakuri a bk _) = fmap bk (f a)
{-# INLINE surface #-}

instance Monad m => Functor (Karakuri m a) where
    fmap = rmap
    {-# INLINE fmap #-}

instance Monad m => Applicative (Karakuri m i) where
    pure a = let k = Karakuri a (const k) (return k) in k
    {-# INLINE pure #-}
    Karakuri f bj mj <*> Karakuri a bk mk = Karakuri (f a) (liftA2 (<*>) bj bk) (liftM2 (<*>) mj mk)

instance Monad m => Comonad (Karakuri m i) where
    extract (Karakuri a _ _) = a
    {-# INLINE extract #-}
    extend f k@(Karakuri a bk mk) = Karakuri (f k) (extend f . bk) (extend f `liftM` mk)

instance Monad m => ComonadApply (Karakuri m i) where
    (<@>) = (<*>)
    {-# INLINE (<@>) #-}

instance Monad m => Profunctor (Karakuri m) where
    rmap ab (Karakuri a bk mk) = Karakuri (ab a) (rmap ab . bk) (rmap ab `liftM` mk)
    lmap cb (Karakuri a bk mk) = Karakuri a (lmap cb . bk . cb) (lmap cb `liftM` mk)

-- | 'join' diagonalizes nested 'Karakuri'.
instance (Distributive m, Monad m) => Monad (Karakuri m i) where
    return a = let k = Karakuri a (const k) (return k) in k
    {-# INLINE return #-}
    k >>= f = go $ fmap f k where
        go (Karakuri a ik mk) = Karakuri (look a) (ik >>= liftM go . collect feed) (mk >>= liftM go . collect step)
    {-# INLINE (>>=) #-}

iterateM :: Monad m => (a -> m a) -> a -> Karakuri m a a
iterateM f = let k a = Karakuri a k (k `liftM` f a) in k
{-# INLINE iterateM #-}
