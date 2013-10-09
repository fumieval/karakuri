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

data ButaiBase m a where
    Register :: Karakuri m r -> ButaiBase m (Key r)
    Look :: Key r -> ButaiBase m r
    UpdateAll :: ButaiBase m ()
makeSingletons ''ButaiBase

newtype ButaiT m a = ButaiT { unButaiT :: ReifiedProgramT (ButaiBase (ButaiT m)) m a } deriving (Monad, Applicative, Functor)


instance MonadIO m => MonadIO (ButaiT m) where
    liftIO = lift . liftIO

instance MonadTrans ButaiT where
    lift = ButaiT . lift

instance Monad m => ButaiBase (ButaiT m) :! ButaiT m where
    singleton = ButaiT . singleton

instance MonadState s m => MonadState s (ButaiT m) where
    get = lift get
    put = lift . put

transButaiBase :: (forall a. m a -> n a) -> ButaiBase m a -> ButaiBase n a
transButaiBase t (Register k) = Register (transKarakuri t k)
transButaiBase _ (Look k) = Look k
transButaiBase _ UpdateAll = UpdateAll

transButaiT :: (Monad m, Monad n) => (forall x. m x -> n x) -> ButaiT m a -> ButaiT n a
transButaiT t = ButaiT . hoistReifiedT (transButaiBase (transButaiT t)) . transReifiedT t . unButaiT

data Any

runButaiT :: forall m a. Monad m => ButaiT m a -> m a
runButaiT = go 0 IM.empty . unButaiT where
    go :: Int -> IM.IntMap (Karakuri (ButaiT m) Any) -> ReifiedProgramT (ButaiBase (ButaiT m)) m a -> m a
    go i m (Register k :>>= cont) = go (succ i) (IM.insert i (unsafeCoerce k) m) $ cont (Kao i)
    go i m (Look k@(Kao j) :>>= cont) = go i m $ cont $ extract (unsafeCoerce (m IM.! j) `asKarakuriOf` k)
    go i m (UpdateAll :>>= cont) = do
        rs <- runButaiT $ forM (IM.toAscList m) (\(i, m) -> (,) i `liftM` step m)
        go i (IM.fromAscList rs) (cont ())
    go i m (Lift a cont) = a >>= go i m . cont
    go _ _ (Return a) = return a

    asKarakuriOf :: Karakuri m x -> p x -> Karakuri m x
    asKarakuriOf x _ = x
