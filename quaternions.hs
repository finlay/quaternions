{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{- LANGUAGE MultiParamTypeClasses #-}
import Vector

data H  -- identity

instance Span H where
    type Dimension H  = 4 
    type ScalarType H = Double
    data BasisType H  = E | I | J | K deriving Eq
    basis = [E, I, J, K]

instance Show (BasisType H) where
    show E = "e"
    show I = "i"
    show J = "j"
    show K = "k"

e,i,j,k :: Elem H
[e,i,j,k] = elements cannonical
