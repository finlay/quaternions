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
mu = extend mu'
      where mu' :: BasisType (Tensor H H) -> Elem H
            mu' (BTensor E E) = e
            mu' (BTensor E I) = i
            mu' (BTensor E J) = j
            mu' (BTensor E K) = k
            mu' (BTensor I E) = i
            mu' (BTensor J E) = j
            mu' (BTensor K E) = k
            mu' (BTensor I J) = k
            mu' (BTensor J K) = i
            mu' (BTensor K I) = j
            mu' (BTensor J I) = -k
            mu' (BTensor K J) = -i
            mu' (BTensor I K) = -j
            mu' (BTensor I I) = -e
            mu' (BTensor J J) = -e
            mu' (BTensor K K) = -e

instance Algebra H where
    unit    = e
    mul x y = mu (x `tensor` y)

