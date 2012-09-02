{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- LANGUAGE DataKinds #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE MultiParamTypeClasses #-}
import Vector

import Text.Printf
import Data.List

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
[e,i,j,k] = canonical

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

comm a b = a * b - b * a
ehh = canonical :: [Elem (Tensor H H)]

sym = [ x `tensor` y + y `tensor` x | x <- canonical, y <- canonical, x <= y ] :: [Elem (Tensor H H)]

ske = [ x `tensor` y - y `tensor` x | x <- canonical, y <- canonical, x < y ] :: [Elem (Tensor H H)]

showProd a b = 
    [ (printf "[%18s ,%18s ] = " (show x) (show y))  ++ show (comm x y) | x <- a, y <- b, x < y]

elms = [comm x y | x <- ehh, y <- ehh, x < y]
-- elms = [ printf "[%15s,%15s] = %-15s" (show x) (show y) (show (comm x y)) 
--        | x <- ehh, y <- ehh ]





