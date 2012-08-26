{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Vector where

import GHC.TypeLits

--import Data.Array.Repa
import qualified Data.Vector.Unboxed as V

class VectorSpace v where
    data Elem v :: *
    type Scalar v :: *
    zero   :: Elem v
    minus  :: Elem v -> Elem v
    plus   :: Elem v -> Elem v -> Elem v
    scale  :: Scalar v -> Elem v -> Elem v

-- Working around the terrible Num class here
instance (VectorSpace v) => Num (Elem v) where
    (+) v w  = plus v w
    (-) v w  = plus v (minus w)
    negate w = minus w

    (*) v w     = undefined
    abs         = undefined
    signum      = undefined
    fromInteger i = if i == 0 then zero else undefined

-- Bases are encoded depending on the particular structure of v
class VectorSpace v => HasBasis v where
    data Basis v :: *
    cannonical    :: Basis v 
    elements      :: Basis v -> [Elem v]
    labels        :: Basis v -> [String]
    coefficients  :: Basis v -> Elem v -> [Scalar v]

showInBasis :: (HasBasis v, RealFrac (Scalar v), Show (Scalar v)) 
            => Basis v -> Elem v -> String
showInBasis basis v = 
    let coef = coefficients basis v
        pairs = zip (labels basis) coef 
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



-- Encode the dimension at the type level.
-- The dimension is used to create the zero element.
class Span v where
    type Dimension v :: Nat
    type ScalarType v :: *
    type BasisType v :: *
    basis :: (Show (BasisType v)) => [BasisType v]

instance (V.Unbox (ScalarType v), Num (ScalarType v), SingI (Dimension v), Span v) 
    => VectorSpace v where
    data Elem v = EL (V.Vector (ScalarType v))
    type Scalar v = ScalarType v
    zero                 = let mkZero :: Sing (Dimension v) -> Elem v
                               mkZero = EL . (flip V.replicate 0) . (fromInteger . fromSing)
                           in  withSing mkZero
    minus (EL v)         = EL $ V.map negate v
    plus  (EL v) (EL w)  = EL $ V.zipWith (+) v w
    scale n (EL v)       = EL $ V.map (n*) v


instance (Span v, V.Unbox (ScalarType v), SingI (Dimension v), Num (ScalarType v)) 
    => HasBasis v where
    data Basis v = Basis [(Elem v, BasisType v)] 
    elements (Basis bs) = map fst bs
    labels   (Basis bs) = map snd bs 
    coefficients b e =
        let dot (EL v) (EL w) =  V.sum $ V.zipWith (*) v w
        in  map (dot e) (elements b)
    cannonical = undefined







cannonical2 = 
    let cbwn :: SingI d => [BasisType v] -> Sing d -> Basis v
        cbwn bs d = 
            let dim = fromInteger $ fromSing d
                es = map mke [1 .. dim]
                delta i j = if i == j then 1 else 0
                mke i = EL $ V.generate dim (delta (i-1)) -- numbering from zero
            in  Basis $ zip es bs
    in withSing ( cbwn basis )

-- -- Now need to define linear maps as extensions of a basis
-- extend :: (HasBasis v, VectorSpace w, Scalar v ~ Scalar w) 
--        => Basis v -> (Elem v -> Elem w) -> (Elem v -> Elem w)
-- extend b f v = 
--     let c = coefficients b v 
--         ws = map f (elements b)
--     in  foldr1 plus $ map (uncurry scale) $ zip c ws
-- 
-- -- Now we can do the tensor product thing.
-- class (VectorSpace v, VectorSpace w, Scalar v ~ Scalar w,
--         HasBasis v, HasBasis w, Num (Scalar w)) 
--         => TensorProd v w where
--     type Tensor v w :: *
--     tensor :: (Scalar (Tensor v w) ~ Scalar v, HasBasis (Tensor v w)) 
--            => Elem v -> Elem w -> Elem (Tensor v w)
--     tensor v w = 
--         let vs  = coefficients cannonicalBasis v
--             ws  = coefficients cannonicalBasis w
--             vws = [ vsc * wsc | vsc <- vs, wsc <- ws ]
--         in  foldr1 plus $ map (uncurry scale) $ zip vws (elements cannonicalBasis)
-- 
-- instance (V.Unbox e, Num e)
--          => TensorProd (Span 4 e) (Span 4 e) where
--     type Tensor (Span 4 e) (Span 4 e) = Span 16 e
-- 
