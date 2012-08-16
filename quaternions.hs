{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
import Vector

type H = Span 4 Double

bH = cannonicalBasisWithNames ["e","i","j","k"] (undefined :: H)

[e,i,j,k] = elements bH

instance Show (Elem H) where
    show = showInBasis bH

instance Num (Elem H) where
    (+) v w = plus v w
    (*) v w = undefined
    (-) v w = plus v (minus w)
    negate w = minus w
    abs = undefined
    signum = undefined
    fromInteger = undefined

