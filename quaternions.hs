{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
import Vector

h = undefined :: Span 4 Double

bH = cannonicalBasisWithNames ["e","i","j","k"] h

[e,i,j,k] = elements bH

instance Show (Elem (Span 4 Double)) where
    show = showInBasis bH

instance Num (Elem (Span 4 Double)) where
    (+) v w = plus v w
    (*) v w = undefined
    (-) v w = plus v (minus w)
    negate w = minus w
    abs = undefined
    signum = undefined
    fromInteger = undefined

