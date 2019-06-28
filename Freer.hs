{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Freer where

newtype Freer f a = Freer {
    runFreer :: forall m. Monad m => (forall x. f x -> m x) -> m a
}

instance Monad (Freer f) where
    return x = Freer $ \_ -> return x
    fa >>= a2fb = Freer $ \natTrans -> do
       a <- runFreer fa natTrans
       runFreer (a2fb a) natTrans

instance Applicative (Freer f) where
    pure = return
    ff <*> fx = Freer $ \natTrans -> do
        f <- runFreer ff natTrans
        x <- runFreer fx natTrans
        return $ f x

instance Functor (Freer f) where
    fmap f fx = Freer $ \natTrans -> f <$> runFreer fx natTrans

liftFreer :: f a -> Freer f a
liftFreer x = Freer $ \natTrans -> natTrans x
