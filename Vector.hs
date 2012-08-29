{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE DataKinds #-}
{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE StandaloneDeriving #-}
{- LANGUAGE KindSignatures #-}
module Vector where

import           Data.List (elemIndex)

--import Data.Array.Repa
import qualified Data.Vector.Unboxed as V

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

instance (V.Unbox (Scalar v), Num (Scalar v), Span v) 
    => VectorSpace v where
    data Elem v = EL (V.Vector (Scalar v))
    zero                 = let bs = basis :: [BasisType v]
                           in  EL $ V.replicate (length bs) 0 
    minus (EL v)         = EL $ V.map negate v
    plus  (EL v) (EL w)  = EL $ V.zipWith (+) v w
    scale n (EL v)       = EL $ V.map (n*) v

-- Bases are encoded depending on the particular structure of v
class VectorSpace v => HasBasis v where
    data Basis v :: *
    embed         :: BasisType v -> Elem v
    canonical     :: Basis v 
    elements      :: Basis v -> [Elem v]
    labels        :: Basis v -> [String]
    coefficients  :: Basis v -> Elem v -> [Scalar v]

instance (Span v, V.Unbox (Scalar v), Num (Scalar v), 
            Show (BasisType v), Eq (BasisType v)) 
    => HasBasis v where
    data Basis v = Basis [(Elem v, BasisType v)] 
    embed b = 
        let bs = basis :: [BasisType v]
            delta i j = if i == j then 1 else 0
        in case elemIndex b basis of
            Just i  -> EL $ V.generate (length bs) (delta i)
            Nothing -> error $ "Element " ++ show b ++ " not in basis?"
    elements (Basis bs) = map fst bs
    labels   (Basis bs) = map (show . snd) bs 
    coefficients b e =
        let dot (EL v) (EL w) =  V.sum $ V.zipWith (*) v w
        in  map (dot e) (elements b)
    canonical = 
        let bs = basis :: [BasisType v]
            dim = length bs
            es = map mke [1 .. dim]
            delta i j = if i == j then 1 else 0
            mke i = EL $ V.generate dim (delta (i-1)) -- numbering from zero
        in  Basis $ zip es bs



instance (VectorSpace v, HasBasis v, RealFrac (Scalar v), Show (Scalar v)) 
            => Show (Elem v) where
    show v = 
        let bs = canonical :: Basis v
            coef = coefficients bs v
            pairs = zip (labels bs) coef 
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

-- TENSOR PRODUCTS
-- Now we can do the tensor product thing.
data Tensor v w
tensor :: (HasBasis v, HasBasis w, HasBasis (Tensor v w), Scalar v ~ Scalar w, Num(Scalar v))
       => Elem v -> Elem w -> Elem (Tensor v w)
tensor v w = 
    let vs  = coefficients canonical v
        ws  = coefficients canonical w
        vws = [ vsc * wsc | vsc <- vs, wsc <- ws ]
    in  foldr1 plus $ map (uncurry scale) $ zip vws (elements canonical)

instance (Span v, Span w) => Span (Tensor v w) where
    type Scalar (Tensor v w)  = Scalar v
    data BasisType  (Tensor v w)  = BTensor (BasisType v) (BasisType w)
    basis = [BTensor bv bw | bv <- basis, bw <- basis]

instance (Show (BasisType v), Show (BasisType w))
    => Show (BasisType (Tensor v w)) where
    show (BTensor bv bw) = show bv ++ "\x2297 " ++ show bw

instance (Eq (BasisType v), Eq (BasisType w))
    => Eq (BasisType (Tensor v w)) where
    BTensor bv bw == BTensor bv' bw' = bv == bv' && bw == bw'

-- HOM SPACES
-- embody the hom space as a vector space
data Hom v w
hom :: (HasBasis v, HasBasis w, HasBasis (Hom v w), Scalar v ~ Scalar w, Num(Scalar v))
       => Elem v -> Elem w -> Elem (Hom v w)
hom v w = 
    let vs  = coefficients canonical v
        ws  = coefficients canonical w
        vws = [ vsc * wsc | vsc <- vs, wsc <- ws ]
    in  foldr1 plus $ map (uncurry scale) $ zip vws (elements canonical)

instance (Span v, Span w) => Span (Hom v w) where
    type Scalar (Hom v w)  = Scalar v
    data BasisType  (Hom v w)  = BHom (BasisType v) (BasisType w)
    basis = [BHom bv bw | bv <- basis, bw <- basis]

instance (Show (BasisType v), Show (BasisType w))
    => Show (BasisType (Hom v w)) where
    show (BHom bv bw) = show bv ++ " \x21A6 " ++ show bw

instance (Eq (BasisType v), Eq (BasisType w))
    => Eq (BasisType (Hom v w)) where
    BHom bv bw == BHom bv' bw' = bv == bv' && bw == bw'

apply :: (Span v, Span w, Scalar v ~ Scalar w,
          V.Unbox (Scalar w), Num (Scalar w)) 
      => Elem (Hom v w) -> Elem v -> Elem w
apply f v = 
    let EL fe = f -- length fe = dv * dw
        EL ve = v -- length ve = dv
        dv = V.length ve
        m i = V.foldr1 (+) $ V.zipWith (*) ve (V.slice i (i + dv) fe)
        res = V.generate (V.length fe `div` dv) m
    in  EL res

-- materialise a linear map into a Hom element
materialise :: (Span v, Span w, Scalar v ~ Scalar w, 
                Num (Scalar w), V.Unbox (Scalar w), 
                Show (BasisType v), Eq (BasisType v),
                Show (BasisType w), Eq (BasisType w)) 
            => (BasisType v -> Elem w) -> Elem (Hom v w)
materialise f = 
    let fv = map f basis
        v  = elements canonical
    in  foldr1 plus . map (uncurry hom) $ zip v fv


-- We can define linear maps by extending the map from a basis
extend :: (Span v, Span w, Scalar v ~ Scalar w, 
           Num (Scalar w), V.Unbox (Scalar w), 
           Show (BasisType w), Eq (BasisType w),
           Show (BasisType v), Eq (BasisType v)) 
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



