{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Vector where

import GHC.TypeLits

--import Data.Array.Repa
import qualified Data.Vector.Unboxed as V

class Additive v where 
    zero   :: v
    minus  :: v -> v
    plus   :: v -> v -> v


-- Need to encode the dimension at both the type and data level.
-- Dimension is used to create the zero element.
-- Also want a unique identity, to statically ensure that linear
--   maps are applied correctly.
data LinearSpan ( d :: Nat ) elm  
data family Elem a :: *
data instance Elem (LinearSpan d e) = EL (V.Vector e)

instance (V.Unbox e, Num e) => Additive (Elem (LinearSpan d e)) where
    zero   = let mkZero :: Sing d -> V.Vector e
                 mkZero d = V.replicate (fromSing d) 0
		     in  EL (withSing mkZero)
    minus  = undefined
    plus   = undefined
