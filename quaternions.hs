{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- LANGUAGE DataKinds #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE MultiParamTypeClasses #-}
import Vector

data H  -- identity

instance Span H where
    type Scalar H = Double
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
  where
    mu' :: BasisType (Tensor H H) -> Elem H
    mu' (BTensor E b) = embed b
    mu' (BTensor b E) = embed b
    mu' (BTensor I J) = k
    mu' (BTensor J K) = i
    mu' (BTensor K I) = j
    mu' (BTensor J I) = minus k
    mu' (BTensor K J) = minus i
    mu' (BTensor I K) = minus j
    mu' (BTensor I I) = minus e
    mu' (BTensor J J) = minus e
    mu' (BTensor K K) = minus e

instance Algebra H where
    unit    = e
    mul x y = mu (x `tensor` y)

-- Now lets make Tensor H H an algebra
instance Algebra (Tensor H H) where
    unit = e `tensor` e
    mul x y = extend muHH (x `tensor` y)
            where 
                muHH  (BTensor (BTensor x y) (BTensor x' y')) 
                    = ((embed x) * (embed x')) `tensor` ((embed y') * (embed y))

-- muHH' :: BasisType (Tensor (Tensor H H) (Tensor H H)) -> Elem (Tensor H H)
-- muHH'  (BTensor (BTensor x y) (BTensor x' y')) 
--     = ((embed x) * (embed x')) `tensor` ((embed y') * (embed y))
-- 
elms = [ (embed x * embed y)  - (embed y * embed x) | x <- basis, y <- basis ] :: [Elem (Tensor H H)]
