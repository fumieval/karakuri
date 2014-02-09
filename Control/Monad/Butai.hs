{-# LANGUAGE Rank2Types, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Butai (ButaiT, register, look, updateAll, Key(..), transButaiT, runButaiT) where

import Data.Karakuri
import Unsafe.Coerce
import Control.Monad.Trans.Operational.Mini
import Control.Monad.Operational.TH
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import qualified Data.IntMap as IM
import Control.Comonad
import Control.Monad.Trans.Class

newtype Key a = Kao Int

data ButaiBase f m a where
    Register :: f r -> ButaiBase f m (Key r)
    Look :: Key r -> ButaiBase f m (f r)
    Act :: (forall r. f r -> m (f r)) -> ButaiBase f m ()

register :: (ButaiBase f (ButaiT f m) :! n) => f r -> n (Key r)
register f = singleton $ Register f

look :: (ButaiBase f (ButaiT f m) :! n) => Key r -> n (f r)
look k = singleton $ Look k

act :: (ButaiBase f (ButaiT f m) :! n) => (forall r. f r -> m (f r)) -> n ()
act f = singleton $ Act f

newtype ButaiT f m a = ButaiT { unButaiT :: ReifiedProgramT (ButaiBase f (ButaiT f m)) m a } deriving (Monad, Applicative, Functor)

instance MonadIO m => MonadIO (ButaiT f m) where
    liftIO = lift . liftIO

instance MonadTrans (ButaiT f) where
    lift = ButaiT . lift

instance Monad m => ButaiBase f (ButaiT f m) :! ButaiT f m where
    singleton = ButaiT . singleton

instance MonadState s m => MonadState s (ButaiT f m) where
    get = lift get
    put = lift . put

transButaiBase :: (forall a. m a -> n a) -> ButaiBase f m a -> ButaiBase f n a
transButaiBase t (Register k) = Register (transKarakuri t k)
transButaiBase _ (Look k) = Look k
transButaiBase t (Act f) = Act (t . f)

transButaiT :: (Monad m, Monad n) => (forall x. m x -> n x) -> ButaiT f m a -> ButaiT f n a
transButaiT t = ButaiT . hoistReifiedT (transButaiBase (transButaiT t)) . transReifiedT t . unButaiT

data Any

runButaiT :: forall f m a. Monad m => ButaiT f m a -> m a
runButaiT = go 0 IM.empty . unButaiT where
    go :: Int -> IM.IntMap (f (ButaiT f m) Any) -> ReifiedProgramT (ButaiBase (ButaiT f m)) m a -> m a
    go i m (Register k :>>= cont) = go (succ i) (IM.insert i (unsafeCoerce k) m) $ cont (Kao i)
    go i m (Look k@(Kao j) :>>= cont) = go i m $ cont $ extract (unsafeCoerce (m IM.! j) `asKarakuriOf` k)
    go i m (Act f :>>= cont) = do
        rs <- runButaiT $ forM (IM.toAscList m) (\(i, m) -> (,) i `liftM` f m)
        go i (IM.fromAscList rs) (cont ())
    go i m (Lift a cont) = a >>= go i m . cont
    go _ _ (Return a) = return a

    asKarakuriOf :: f m x -> p x -> f m x
    asKarakuriOf x _ = x
