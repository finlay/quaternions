{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Extensive where

import Control.Monad (join)

type R = Double
newtype V a = V { unV :: ((a -> R) -> R) }

instance Monad V where
    return x      = V $ flip id x
    (V x) >>= y   = V $ x . flip ( unV . y )

instance Functor V where 
    fmap f (V xs) = V $ xs . (flip ((flip id) . f))


tensor :: V a -> V b -> V (Tensor a b)
tensor tx ty =  (fmap (\(x,y) -> Tensor x y) . join . (fmap t') . t'') (tx, ty)
    where
        t'' (x, y) = fmap (x,) y
        t'  (x, y) = fmap (,y) x

data Tensor a b = Tensor a b

class FiniteSet x where elements :: [ x ]
instance (FiniteSet x, FiniteSet y) => FiniteSet (Tensor x y) where
    elements = [ Tensor a b | a <- elements, b <- elements ]

instance (Show x, Show y) => Show (Tensor x y) where
    show (Tensor x y) = show x ++ " \x2297 " ++ show y

instance (Eq x, Eq y) => Eq (Tensor x y) where
    Tensor x y == Tensor x' y' = x == x' && y == y'

instance (Ord x, Ord y) => Ord (Tensor x y) where
    Tensor x y <= Tensor x' y' = x <= x' && y <= y'

instance (Eq a, FiniteSet a) => Eq (V a) where
    x == y = (coefficients x) == (coefficients y)

instance (Eq a, FiniteSet a, Ord a) => Ord (V a) where
    compare x y = compare (coefficients x) (coefficients y)

extend :: (Monad m) => (a -> m b) -> m a -> m b
extend = flip (>>=)

delta :: (Eq x) => x -> x -> R
delta a b = if a == b then 1 else 0

-- Basis
class Basis b x where
    eta   :: b -> x -> R
    coef  :: V x -> b -> R
    coef (V v) = v . eta

-- canonical basis
instance (Eq x) => Basis x x where
    eta = delta

coefficients :: (FiniteSet x, Eq x) => V x -> [(x, R)]
coefficients (V v) = map (\e -> (e, v (delta e))) elements

showInBasis :: (Show b, Eq b, Basis b x) 
            => [b] -> V x -> String
showInBasis bs v =
        let pairs = map (\e -> (e, coef v e)) bs
            showPair (b, n) 
               | n == 1.0    = " + "                  ++ show b
               | n == -1.0   = " - "                  ++ show b
               | n > 0       = " + " ++ showN n       ++ show b
               | otherwise   = " - " ++ showN (abs n) ++ show b
            showN n = if n == fromInteger (round n) 
                       then show (round n) 
                       else show n
        in  case map showPair . filter (\(_,n) -> n /= 0.0) $ pairs of 
                  [] -> " 0"
                  ss -> concat ss


instance (Eq a, FiniteSet a, Show a) => Show (V a) where
    show = let showInCanonicalBasis :: (Show a, Eq a) => [a] -> V a -> String
               showInCanonicalBasis = showInBasis 
           in  showInCanonicalBasis elements

zero :: V a
zero = V $ \_ -> 0

minus :: V a -> V a
minus (V x) = V $ \f -> - (x f)

plus :: V a -> V a -> V a
plus (V x) (V y) = V $ (\ar -> x ar + y ar)

scale :: R  -> V a -> V a
scale r (V x) = V $(\ar -> r * (x ar))

-- ALGEBRAS (Associative)
class Algebra x where
    unit :: x
    mul  :: x -> x -> x

-- Working around the terrible Num class here
instance Algebra (V v) => Num (V v) where
    (+) v w        = plus v w
    (-) v w        = plus v (minus w)
    negate w       = minus w
    (*) v w        = mul v w
    abs            = undefined
    signum         = undefined
    fromInteger i  = scale (fromInteger i) unit

