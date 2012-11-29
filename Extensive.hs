module Extensive where

type R = Double

newtype V a = V { unV :: ((a -> R) -> R) }

instance Monad V where
    return x      = V $ flip id x
    (V x) >>= y   = V $ x . flip ( unV . y )

instance Functor V where 
    fmap f (V xs) = V $ xs . (flip ((flip id) . f))

u x y = (x,y)

t'' (x, y) = fmap (u x) y


data H = E | I | J | K deriving (Eq, Ord, Show)









zero :: V a
zero = V $ \_ -> 0

minus :: V a -> V a
minus (V x) = V $ \f -> - (x f)

plus :: V a -> V a -> V a
plus (V x) (V y) = V $ (\ar -> x ar + y ar)
