module Common.Effects.Noop where

class Functor f => Noop f where
    noop :: Monad m => f (m a) -> m a