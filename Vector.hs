{- LANGUAGE TypeFamilies #-}
{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE FlexibleInstances #-}
{- LANGUAGE UndecidableInstances #-}
module Vector where

import Data.Array.Repa

class AdditiveGroup v where 
    data Elem v :: *
    dimension :: v -> Int
    zero   :: Elem v
    minus  :: Elem v -> Elem v
    plus   :: Elem v -> Elem v -> Elem v

-- Need to encode the dimension at both the type and data level.
-- Dimension is used to create the zero element.
-- Also want a unique identity, to statically ensure that linear
--   maps are applied correctly.
data Nat = Zero | Succ Nat

data Rag r e
instance (Source r e) => AdditiveGroup (Rag r e) where
    newtype Elem (Rag d) = R (Shape r DIM1 e)
    dimension (Rag d) = d
    zero    = R $ fromFunction (Z :. (4 :: Int)) (const 0.0)
    minus v =  
    
