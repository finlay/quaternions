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

--import Data.Array.Repa
--import qualified Data.Vector.Unboxed as V
import qualified Data.Packed.Vector as V
import qualified Numeric.Container as V
import           Foreign.Storable (Storable)

-- Hold the types together 
class Span v where
    type Scalar v :: *
    data BasisType v :: *
    basis :: [BasisType v]

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
    zero                 = let bs = basis :: [BasisType v]
                           in  EL $ V.constant 0 (length bs) 
    minus (EL v)         = EL $ V.scale (-1) v
    plus  (EL v) (EL w)  = EL $ V.add v w
    scale n (EL v)       = EL $ V.scale n v

instance (Ord (Scalar v), Span v, V.Element (Scalar v), V.Container V.Vector (Scalar v))
    => Ord (Elem v) where
    compare (EL v) (EL w) = compare v w

-- Bases are encoded depending on the particular structure of v
class VectorSpace v => HasBasis v where
    embed         :: (Eq (BasisType v), Show (BasisType v)) 
                  => BasisType v -> Elem v
    canonical     :: [Elem v]
    coefficients  :: Elem v -> [Scalar v]

instance (V.Product (Scalar v), V.Container V.Vector (Scalar v), Span v)
    => HasBasis v where
    embed b = 
        let bs = basis :: [BasisType v]
            delta i j = if i == j then 1 else 0
        in case elemIndex b basis of
            Just i  -> EL $ V.buildVector (length bs) (delta i)
            Nothing -> error $ "Element " ++ show b ++ " not in basis?"
    coefficients (EL e) = V.toList e
    canonical = 
        let bs = basis :: [BasisType v]
            dim = length bs
            es = map mke [1 .. dim]
            delta i j = if i == j then 1 else 0
            mke i = EL $ V.buildVector dim (delta (i-1)) -- numbering from zero
        in  es 


instance (VectorSpace v, HasBasis v, Show (BasisType v),
          RealFrac (Scalar v), Show (Scalar v)) 
            => Show (Elem v) where
    show v = 
        let bs = basis :: [BasisType v]
            coef = coefficients v
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
data Tensor v w
tensor :: (V.Product (Scalar v), Scalar v ~ Scalar w)
       => Elem v -> Elem w -> Elem (Tensor v w)
tensor (EL v) (EL w) = EL $ V.flatten $ V.outer v w

instance (Span v, Span w) => Span (Tensor v w) where
    type Scalar (Tensor v w)  = Scalar v
    data BasisType  (Tensor v w)  = BTensor (BasisType v) (BasisType w)
    basis = [BTensor bv bw | bv <- basis, bw <- basis]

instance (Show (BasisType v), Show (BasisType w))
    => Show (BasisType (Tensor v w)) where
    show (BTensor bv bw) = show bv ++ " \x2297 " ++ show bw

instance (Eq (BasisType v), Eq (BasisType w))
    => Eq (BasisType (Tensor v w)) where
    BTensor bv bw == BTensor bv' bw' = bv == bv' && bw == bw'

-- HOM SPACES
-- embody the hom space as a vector space
data Hom v w
hom :: (V.Product (Scalar v), Scalar v ~ Scalar w)
       => Elem v -> Elem w -> Elem (Hom v w)
hom (EL v) (EL w) = EL $ V.flatten $ V.outer w v

instance (Span v, Span w) => Span (Hom v w) where
    type Scalar (Hom v w)  = Scalar v
    data BasisType  (Hom v w)  = BHom (BasisType v) (BasisType w)
    basis = [BHom bv bw | bw <- basis, bv <- basis]

instance (Show (BasisType v), Show (BasisType w))
    => Show (BasisType (Hom v w)) where
    show (BHom bv bw) = show bv ++ " \x21A6 " ++ show bw

instance (Eq (BasisType v), Eq (BasisType w))
    => Eq (BasisType (Hom v w)) where
    BHom bv bw == BHom bv' bw' = bv == bv' && bw == bw'

apply :: (Span v, Span w, Scalar v ~ Scalar w, V.Product (Scalar v)) 
      => Elem (Hom v w) -> Elem v -> Elem w
apply (EL f) (EL v) = EL $ (V.reshape (V.dim v) f) `V.mXv` v 

-- materialise a linear map into a Hom element
materialise :: (Span v, Span w, Scalar v ~ Scalar w, V.Product (Scalar w),
                V.Container V.Vector (Scalar v))
            => (BasisType v -> Elem w) -> Elem (Hom v w)
materialise f = 
    let fv = map f basis
        v  = canonical
    in  foldl1 plus . map (uncurry hom) $ zip v fv


-- We can define linear maps by extending the map from a basis
extend :: (Span v, Span w, Scalar v ~ Scalar w, 
           V.Product (Scalar v), V.Container V.Vector (Scalar v))
       => (BasisType v -> Elem w) -> Elem v -> Elem w
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



