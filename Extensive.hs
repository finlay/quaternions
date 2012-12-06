{-# LANGUAGE TupleSections #-}

module Extensive where

import Control.Monad (join)

type R = Double
newtype V a = V { unV :: ((a -> R) -> R) }

instance Monad V where
    return x      = V $ flip id x
    (V x) >>= y   = V $ x . flip ( unV . y )

instance Functor V where 
    fmap f (V xs) = V $ xs . (flip ((flip id) . f))


tensor :: V a -> V a -> V (a, a)
tensor tx ty =  (join . (fmap t') . t'') (tx, ty)
    where
        t'' (x, y) = fmap (x,) y
        t'  (x, y) = fmap (,y) x

class FiniteSet x where 
    elements :: [ x ]
data H = E | I | J | K deriving (Eq, Ord, Show)
instance FiniteSet H where elements = [ E, I, J, K ]

instance (FiniteSet x, FiniteSet y) => FiniteSet (x,y) where
    elements = [ (a,b) | a <- elements, b <- elements ]

delta :: (Eq x) => x -> x -> R
delta a b = if a == b then 1 else 0

coef :: (FiniteSet x, Eq x) => V x -> [R]
coef v = map (unV v . delta ) elements

e = return E :: V H
i = return I :: V H
j = return J :: V H
k = return K :: V H








zero :: V a
zero = V $ \_ -> 0

minus :: V a -> V a
minus (V x) = V $ \f -> - (x f)

plus :: V a -> V a -> V a
plus (V x) (V y) = V $ (\ar -> x ar + y ar)
