{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Extensive where

import Control.Monad (join)
import Text.Printf

import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC


--type R = Rational ; epsilon = 0 -- slow and accurate
type R = Double ; epsilon = 1e-6 -- fast and approximate
show' r = printf "%0.4f" $ if abs r < epsilon then 0 else r


newtype V a = V { unV :: ((a -> R) -> R) }

instance Functor V where 
    fmap f (V xs) = V $ xs . (flip ((flip id) . f))

instance Monad V where
    return x      = V $ flip id x
    (V x) >>= y   = V $ x . flip ( unV . y )


-- Tensor products are just pairs
data Tensor a b = Tensor a b deriving (Eq, Ord)
tensor :: V a -> V b -> V (Tensor a b)
tensor = curry $ fmap (uncurry Tensor) . join . (fmap t') . t''
    where
        t'' (x, y) = fmap (x,) y
        t'  (x, y) = fmap (,y) x


data Hom a b = Hom a b deriving (Eq, Ord)
hom  :: (V a -> V b) -> V (Hom a b)
hom  = undefined 
eval :: V (Hom a b) -> V a -> V b
eval = undefined

{--
 (((a, b) -> R) -> R) -> ((a -> R) -> R) -> (b -> R) -> R

 l vx y = 

 (a,) :: b -> (a,b)
 fmap (a,) :: V b -> V (a,b) 

 V (a,b) -> V a -> V b


  em' :: (Eq a) => (a, b) -> (b -> R) -> (a -> R)
  em' (x,y) vy = \x' -> if x' == x then vy b else 0

  em' (x, y) :: (b -> R) -> (a -> R)
  (. em' (x, y)) :: ((a -> R) -> R) -> (b -> R) -> R
                :: V a -> V b

  em :: (Eq a) => (a, b) -> V a -> V b
  em (x, y) = (. (em' (x, y)))

  let em :: (Eq a) => (a, b) -> V a -> V b  
      em (x,y) (V vx) = V $ (vx . (em' (x, y)))
      em' (x,y) vy = \x' -> if x' == x then vy b else 0

so, we have em :: (a, b) -> V a -> V b

--}

em :: (Eq a) => (a, b) -> V a -> V b  
em (x,y) (V vx) = V $ (vx . (em' (x, y)))
  where
    em' (x,y) vy = \x' -> if x' == x then vy y else 0


-- Finite sets can be listed, which is elements
class FiniteSet x where elements :: [ x ]

instance (FiniteSet x, FiniteSet y) => FiniteSet (Tensor x y) where
    elements = [ Tensor a b | a <- elements, b <- elements ]

instance (Show x, Show y) => Show (Tensor x y) where
    show (Tensor x y) = show x ++ " \x2297 " ++ show y

delta :: (Eq x) => x -> x -> R
delta a b = if a == b then 1 else 0

-- If we have a vector over a finite set, we can calculate the coefficients
coefficients :: (FiniteSet x, Eq x) => V x -> [(x, R)]
coefficients (V v) = map (\e -> (e, v (delta e))) elements

instance (Eq a, FiniteSet a) => Eq (a -> R) where
    x == y = all (\e -> x e == y e) elements

instance (Eq a, FiniteSet a) => Eq (V a) where
    x == y = sum (map (squared . snd) ( coefficients (subtract x y))) <= epsilon
              where 
                squared x = x * x
                subtract (V x) (V y) = V $ (\ar -> x ar - y ar)

instance (Eq a, FiniteSet a, Ord a) => Ord (V a) where
    compare x y = compare (coefficients x) (coefficients y)


-- Now we can talk about maps
-- Extending a map into a the Vector space is easy peasy using the Monad instance
-- Automatically linear
extend :: (Monad m) => (a -> m b) -> m a -> m b
extend = flip (>>=)

codual :: (Eq a) => V a -> (a -> R)
codual (V x)  = x . delta

dual :: (FiniteSet a, Eq a) => (a -> R) -> V a
dual x = foldl1 plus $ map (\e -> scale (x e) (return e))  elements

dot :: (Eq a, FiniteSet a) => V a -> V a -> R
dot (V y) = y . codual

-- Transpose
transpose :: (FiniteSet a, FiniteSet b, Eq a, Eq b) 
          => (V a -> V b) -> (V b -> V a)
transpose lm = dual . flip (unV . lm . return) . codual


-- want to allow the creation of inverses 
-- assume that the linear map is isometric!
inverse :: (FiniteSet a, FiniteSet b, Eq a, Eq b) 
        => (V a -> V b) -> Maybe (V b -> V a)
inverse lm = Just $ transpose lm




-- Don't really want to use this basis stuff.....
-- Instead just use the linear maps stuff above

-- Basis
class Basis b x where
    eta   :: b -> x -> R
    coef  :: V x -> b -> R
    coef (V v) = v . eta

-- canonical basis
instance (Eq x) => Basis x x where
    eta = delta

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
minus (V x) = V $ negate . x 

plus :: V a -> V a -> V a
plus (V x) (V y) = V $ (\ar -> x ar + y ar)

scale :: R  -> V a -> V a
scale r (V x) = V $ (r *) . x 

-- ALGEBRAS (Associative)
class Algebra x where
    unit :: x
    mul  :: x -> x -> x


instance (Arbitrary a, Algebra (V a)) => Arbitrary (V a)
  where
    arbitrary = 
      do
        bs   <- QC.listOf1 QC.arbitrary
        coef <- QC.vector (length bs)
        return $ foldl1 (+) $ map (\(n,b) -> scale n (return b)) $ zip coef bs


-- Working around the terrible Num class here
instance Algebra (V v) => Num (V v) where
    (+) v w        = plus v w
    (-) v w        = plus v (minus w)
    negate w       = minus w
    (*) v w        = mul v w
    abs            = undefined
    signum         = undefined
    fromInteger i  = scale (fromInteger i) unit

