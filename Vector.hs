{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE DataKinds #-}
{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE StandaloneDeriving #-}
{- LANGUAGE KindSignatures #-}
module Vector where

import           Data.List (elemIndex)

import qualified Data.Packed.Vector as V
import qualified Numeric.Container as V
import           Foreign.Storable (Storable)

-- Hold the types together 
class Span v where
    type Scalar v :: *
    canonical :: [v]

class Span v => VectorSpace v where
    data Elem v :: *
    zero   :: Elem v
    minus  :: Elem v -> Elem v
    plus   :: Elem v -> Elem v -> Elem v
    scale  :: Scalar v -> Elem v -> Elem v

instance (V.Element (Scalar v), V.Container V.Vector (Scalar v),
          Num (Scalar v), Span v) 
    => VectorSpace v where
    newtype Elem v = EL { unEL :: V.Vector (Scalar v) }
    zero                 = let bs = canonical :: [v]
                           in  EL $ V.constant 0 (length bs) 
    minus (EL v)         = EL $ V.scale (-1) v
    plus  (EL v) (EL w)  = EL $ V.add v w
    scale n (EL v)       = EL $ V.scale n v

instance (Ord (Scalar v), Span v, V.Element (Scalar v), V.Container V.Vector (Scalar v))
    => Ord (Elem v) where
    compare (EL v) (EL w) = compare v w

class Basis b where
    type VS b :: *
    basis    :: [b]
    embed    :: (Eq b, VectorSpace (VS b)) => b -> Elem (VS b)
    elements :: (Eq (VS b), VectorSpace (VS b)) => [Elem (VS b)]
    elements  = map embed basis

instance (V.Product (Scalar v), V.Container V.Vector (Scalar v), 
          Span v , a ~  v, Show v)
    => Basis v where
    type VS v = v
    basis = canonical
    embed b = 
        let bs = canonical :: [v]
            delta i j = if i == j then 1 else 0
        in case elemIndex b bs of
            Just i  -> EL $ V.buildVector (length bs) (delta i)
            Nothing -> error $ "Element " ++ show b ++ " not in basis?"

instance (VectorSpace v, Show v, RealFrac (Scalar v), 
            Show (Scalar v), Storable (Scalar v)) 
            => Show (Elem v) where
    show v = 
        let bs = canonical :: [v]
            coef = V.toList $ unEL  v
            pairs = zip (map show bs) coef 
            showPair (b, n) 
               | n == 1.0    = " + "                  ++ b
               | n == -1.0   = " - "                  ++ b
               | n > 0       = " + " ++ showN n       ++ b
               | otherwise   = " - " ++ showN (abs n) ++ b
            showN n = if n == fromInteger (round n) 
                       then show (round n) 
                       else show n
        in  case map showPair . filter (\(_,n) -> n /= 0.0) $ pairs of 
                  [] -> " 0"
                  ss -> concat ss

instance (VectorSpace v, Eq (Scalar v), Storable (Scalar v)) => Eq (Elem v) where
    EL v == EL w = v == w

-- TENSOR PRODUCTS
-- Now we can do the tensor product thing.
data Tensor v w = Tensor v w
tensor :: (V.Product (Scalar v), Scalar v ~ Scalar w)
       => Elem v -> Elem w -> Elem (Tensor v w)
tensor (EL v) (EL w) = EL $ V.flatten $ V.outer v w

instance (Span v, Span w) => Span (Tensor v w) where
    type Scalar (Tensor v w)  = Scalar v
    canonical = [Tensor bv bw | bv <- canonical, bw <- canonical]

instance (Show v, Show w) => Show (Tensor v w) where
    show (Tensor bv bw) = show bv ++ " \x2297 " ++ show bw

instance (Eq v, Eq w) => Eq (Tensor v w) where
    Tensor bv bw == Tensor bv' bw' = bv == bv' && bw == bw'

-- HOM SPACES
-- embody the hom space as a vector space
data Hom v w = Hom v w
hom :: (V.Product (Scalar v), Scalar v ~ Scalar w)
       => Elem v -> Elem w -> Elem (Hom v w)
hom (EL v) (EL w) = EL $ V.flatten $ V.outer w v

instance (Span v, Span w) => Span (Hom v w) where
    type Scalar (Hom v w)  = Scalar v
    canonical = [Hom bv bw | bw <- canonical, bv <- canonical]

instance (Show v, Show w) => Show (Hom v w) where
    show (Hom bv bw) = show bv ++ " \x21A6 " ++ show bw

instance (Eq v, Eq w) => Eq (Hom v w) where
    Hom bv bw == Hom bv' bw' = bv == bv' && bw == bw'

apply :: (Span v, Span w, Scalar v ~ Scalar w, V.Product (Scalar v)) 
      => Elem (Hom v w) -> Elem v -> Elem w
apply (EL f) (EL v) = EL $ (V.reshape (V.dim v) f) `V.mXv` v 

-- materialise a linear map into a Hom element
materialise :: (Span v, Span w, Scalar v ~ Scalar w, Show v, Eq v,
                Basis v, V.Product (Scalar w),
                V.Container V.Vector (Scalar v))
            => (v -> Elem w) -> Elem (Hom v w)
materialise f = 
    let fv = map f basis
        v  = map embed basis
    in  foldr1 plus . map (uncurry hom) $ zip v fv


-- We can define linear maps by extending the map from a basis
extend :: (Span v, Span w, Scalar v ~ Scalar w, Basis v, Show v, Eq v,
           V.Product (Scalar v), V.Container V.Vector (Scalar v))
       => (v -> Elem w) -> Elem v -> Elem w
extend f = apply (materialise f)


-- ALGEBRAS (Associative)
class VectorSpace v => Algebra v where
    unit :: Elem v
    mul  :: Elem v -> Elem v -> Elem v

-- Working around the terrible Num class here
instance (Algebra v, Num (Scalar v)) => Num (Elem v) where
    (+) v w        = plus v w
    (-) v w        = plus v (minus w)
    negate w       = minus w
    (*) v w        = mul v w
    abs            = undefined
    signum         = undefined
    fromInteger i  = scale (fromInteger i) unit



