{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.Coproduct where
import Control.Monad.Free

data (f :+: g) a where
    InL :: f a -> (f :+: g) a
    InR :: g a -> (f :+: g) a
infixr 8 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap h (InL f) = InL $ h <$> f
    fmap h (InR g) = InR $ h <$> g

class (Functor l, Functor g) => l :<: g where
    inj :: l a -> g a

instance (Functor f) => f :<: f where
    inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = InL 

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where 
    inj = InL . inj

inject :: (f :<: g) => f (Free g a) -> Free g a
inject = Free . inj

