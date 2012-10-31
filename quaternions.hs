{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- LANGUAGE DataKinds #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE MultiParamTypeClasses #-}
import Vector

import Text.Printf
import Data.List

data H = E | I | J | K deriving (Eq, Ord)

instance Span H where
    type Scalar H = Double
    canonical = [E, I, J, K]

instance Show H where
    show E = "e"
    show I = "i"
    show J = "j"
    show K = "k"

[e,i,j,k] = elements (canonicalBasis E)

instance Show (Elem H) where
    show = showInBasis (canonicalBasis E)

--mu :: Elem (Tensor H H) -> Elem H
--mu = extend mu'
--  where
--    mu' :: Tensor H H -> Elem H
--    mu' (Tensor E b) = embed b
--    mu' (Tensor b E) = embed b
--    mu' (Tensor I J) = k
--    mu' (Tensor J K) = i
--    mu' (Tensor K I) = j
--    mu' (Tensor J I) = minus k
--    mu' (Tensor K J) = minus i
--    mu' (Tensor I K) = minus j
--    mu' (Tensor I I) = minus e
--    mu' (Tensor J J) = minus e
--    mu' (Tensor K K) = minus e
--
--instance Algebra H where
--    unit    = e
--    mul x y = mu (x `tensor` y)
--
---- Now lets make Tensor H H an algebra
--instance Algebra (Tensor H H) where
--    unit = e `tensor` e
--    mul x y = extend muHH (x `tensor` y)
--            where 
--                muHH  (Tensor (Tensor x y) (Tensor x' y')) 
--                    = ((embed x) * (embed x')) `tensor` ((embed y') * (embed y))
--
--comm a b = a * b - b * a
--ehh = elements :: [Elem (Tensor H H)]
--
--sym = [ x `tensor` y + y `tensor` x 
--      | x <- elements, y <- elements, x <= y ] 
--        :: [Elem (Tensor H H)]
--
--ske = [ x `tensor` y - y `tensor` x 
--      | x <- elements, y <- elements, x < y ] 
--        :: [Elem (Tensor H H)]
--
--showProd a b = 
--    [ (printf "[%18s ,%18s ] = " (show x) (show y))  ++ show (comm x y) 
--        | x <- a, y <- b, x < y]
--
--
---- Create a new more convenient basis for H Tensor H
---- Need to give names, and elements
---- Then, expand arbitary elements in the new basis
--data TauBasis = Sym H H | Skew H H
---- instance Basis TauBasis where
----     type VS TauBasis = Tensor H H
--basisT = [ Sym  a a | a <- [E,I,J,K]]
--      ++ [ Sym  E a | a <- [I,J,K]]
--      ++ [ Sym  J K , Sym  K I, Sym  I J]
--      ++ [ Skew E a | a <- [I,J,K]]
--      ++ [ Skew J K , Skew K I, Skew I J]
--embedT (Sym  a b) = (embed a) `tensor` (embed b) + (embed b) `tensor` (embed a)
--embedT (Skew a b) = (embed a) `tensor` (embed b) - (embed b) `tensor` (embed a)
--
--instance Show TauBasis where
--    show (Sym  a b) = show a ++ " \x2228 " ++ show b
--    show (Skew a b) = show a ++ " \x2227 " ++ show b







