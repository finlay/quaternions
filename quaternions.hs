{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
import Vector

type H = Span 4 Double
basis = ["e","i","j","k"]
basisH = cannonicalBasisWithNames basis :: Basis H

[e,i,j,k] = elements basisH

instance Show (Elem H) where show = showInBasis basisH

type HH = Tensor H H 
basisHH = cannonicalBasisWithNames bs :: Basis HH
    where bs = [ x ++ "\\otimes " ++ y | x <- basis, y <- basis ]


--instance Show (Elem HH) where show = showInBasis basisHH


