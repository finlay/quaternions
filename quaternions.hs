{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Vector

data H  -- identity
instance Span H where
    type Dimension H  = 4 
    type ScalarType H = Double
    type BasisType H  = String
    basis = ["e","i","j","k"]

