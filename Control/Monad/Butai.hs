{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, GADTs #-}
module Control.Monad.Butai where
import Control.Monad.Operational.Mini
import Data.Profunctor
import Data.Proxy
import Data.Karakuri
import Control.Applicative
import qualified Data.IntMap as IM
import Control.Monad.State.Strict
import Data.Traversable as T
import Unsafe.Coerce

data Wire a b where
    Wire :: (a -> s) -> (t -> b) -> Int -> Wire a b

instance Profunctor Wire where
    dimap p q (Wire f g i) = Wire (f . p) (q . g) i
    {-# INLINE dimap #-}

instance Functor (Wire b) where
    fmap = rmap
    {-# INLINE fmap #-}

data ButaiOp m x where
    Lift :: m a -> ButaiOp m a
    Spawn :: Karakuri m a b -> ButaiOp m (Wire a b)
    Kill :: Wire a b -> ButaiOp m ()
    Observe :: Wire a b -> ButaiOp m b
    Affect :: Wire a b -> a -> ButaiOp m ()
    Act :: ButaiOp m ()

newtype ButaiT m a = ButaiT (Program (ButaiOp m) a) deriving (Functor, Applicative, Monad)

instance ButaiOp m :! ButaiT m where
    singleton t = ButaiT (singleton t)
    {-# INLINE singleton #-}

instance MonadTrans ButaiT where
    lift = ButaiT . singleton . Lift

data Any

runButaiT :: Monad m => ButaiT m a -> m a
runButaiT (ButaiT b) = flip evalStateT (0, IM.empty) $ interpret runButaiOp b where

runButaiOp :: Monad m => ButaiOp m a -> StateT (Int, IM.IntMap (Karakuri m Any Any)) m a 
runButaiOp (Lift m) = lift m
runButaiOp (Spawn k) = StateT $ \(i, m) -> return (Wire id id i, (succ i, IM.insert i (unsafeCoerce k) m))
runButaiOp (Kill (Wire _ _ j)) = modify $ \(i, m) -> (i, IM.delete j m)
runButaiOp (Observe (Wire _ tb j)) = do
    (_, m) <- get
    let Karakuri t _ _ = unsafeCoerce $ m IM.! j
    return (tb t)
runButaiOp (Affect (Wire as _ j) a) = modify $ \(i, m) -> (i, IM.adjust (\(Karakuri _ sk _) -> unsafeCoerce sk (as a)) j m)
runButaiOp Act = do
    (i, m) <- get
    m' <- lift $ T.mapM step m
    put (i, m')