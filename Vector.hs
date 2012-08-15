{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

instance (V.Unbox e, Num e, SingI d) => Additive (Elem (LinearSpan d e)) where
    zero                 = let mkZero :: Sing d -> Elem (LinearSpan d e)
                               mkZero = EL . (flip V.replicate 0) . (fromInteger . fromSing)
                           in  withSing mkZero
    minus (EL v)         = EL $ V.map negate v
    plus  (EL v) (EL w)  = EL $ V.zipWith (+) v w

