{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Control.Monad.Butai where
import Control.Monad.Trans.Operational.Mini
import Data.Profunctor
import Data.Proxy
import Data.Karakuri
import Control.Applicative
import qualified Data.IntMap as IM
import Control.Monad.State.Strict
import Data.Traversable as T
import Control.Comonad
import Unsafe.Coerce

data Wire a b where
    Wire :: (a -> s) -> (t -> b) -> Int -> Wire a b

instance Profunctor Wire where
    dimap p q (Wire f g i) = Wire (f . p) (q . g) i
    {-# INLINE dimap #-}

instance Functor (Wire b) where
    fmap = rmap
    {-# INLINE fmap #-}

data ButaiOp k x where
    Spawn :: k a b -> ButaiOp k (Wire a b)
    Kill :: Wire a b -> ButaiOp k ()
    Observe :: Wire a b -> ButaiOp k b
    Affect :: Wire a b -> a -> ButaiOp k ()
    Act :: ButaiOp k ()

spawn :: (ButaiOp k :! m) => k a b -> m (Wire a b)
spawn k = singleton (Spawn k)
{-# INLINE spawn #-}

kill :: (ButaiOp k :! m) => Wire a b -> m ()
kill w = singleton (Kill w)
{-# INLINE kill #-}

observe :: (ButaiOp k :! m) => Wire a b -> m b
observe w = singleton (Observe w)
{-# INLINE observe #-}

affect :: (ButaiOp k :! m) => Wire a b -> a -> m ()
affect w a = singleton (Affect w a)
{-# INLINE affect #-}

act :: (ButaiOp k :! m) => m ()
act = singleton Act
{-# INLINE act #-}

newtype ButaiT m a = ButaiT { unButaiT :: ProgramT (ButaiOp (Karakuri m)) m a } deriving (Functor, Applicative, Monad)

instance ButaiOp (Karakuri m) :! ButaiT m where
    singleton t = ButaiT (singleton t)
    {-# INLINE singleton #-}

instance MonadTrans ButaiT where
    lift = ButaiT . lift

data Any

runButaiT :: Monad m => ButaiT m a -> m a
runButaiT (ButaiT b) = flip evalStateT (0, IM.empty) $ unProgramT b return (join . lift) (\t cont -> runButaiOp t >>= cont)

runButaiOp :: Monad m => ButaiOp (Karakuri m) a -> StateT (Int, IM.IntMap (Karakuri m Any Any)) m a 
runButaiOp (Spawn k) = StateT $ \(i, m) -> return (Wire id id i, (succ i, IM.insert i (unsafeCoerce k) m))
runButaiOp (Kill (Wire _ _ j)) = modify $ \(i, m) -> (i, IM.delete j m)
runButaiOp (Observe (Wire _ tb j)) = do
    (_, m) <- get
    let Karakuri t _ _ = unsafeCoerce $ m IM.! j
    return (tb t)
runButaiOp (Affect (Wire as _ j) a) = modify $ \(i, m) -> (i, IM.adjust (\(Karakuri _ sk _) -> unsafeCoerce sk (as a)) j m)
runButaiOp Act = StateT $ \(i, m) -> do
    m' <- T.mapM step m
    return ((), (i, m'))