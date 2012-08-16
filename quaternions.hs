{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
import Vector

type H = Span 4 Double

basisH = cannonicalBasisWithNames ["e","i","j","k"] :: Basis H

[e,i,j,k] = elements basisH

instance Show (Elem H) where show = showInBasis basisH

