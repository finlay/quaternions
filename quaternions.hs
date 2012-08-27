{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- LANGUAGE DataKinds #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE MultiParamTypeClasses #-}
import Vector

data H  -- identity

instance Span H where
    type ScalarType H = Double
    data BasisType H  = E | I | J | K deriving Eq
    basis = [E, I, J, K]

instance Show (BasisType H) where
    show E = "e"
    show I = "i"
    show J = "j"
    show K = "k"

e,i,j,k :: Elem H
[e,i,j,k] = elements canonical

mu :: Elem (Tensor H H) -> Elem H
mu = extend canonical mu'
      where mu' (tensor e b) = b
            mu' (tensor b e) = b
            mu' (tensor i j) = k
            mu' (tensor j k) = i
            mu' (tensor k i) = j
            mu' (tensor j i) = -k
            mu' (tensor k j) = -i
            mu' (tensor i k) = -j
            mu' (tensor i i) = -e
            mu' (tensor j j) = -e
            mu' (tensor k k) = -e
