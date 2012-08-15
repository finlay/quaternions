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

-- Need to encode the dimension at both the type level.
-- Dimension is used to create the zero element.
data Span ( d :: Nat ) elm  

data family Elem a :: *
data instance Elem (Span d e) = EL (V.Vector e)

instance (V.Unbox e, Num e, SingI d) => Additive (Elem (Span d e)) where
    zero                 = let mkZero :: Sing d -> Elem (Span d e)
                               mkZero = EL . (flip V.replicate 0) . (fromInteger . fromSing)
                           in  withSing mkZero
    minus (EL v)         = EL $ V.map negate v
    plus  (EL v) (EL w)  = EL $ V.zipWith (+) v w


{-
Now to encode the concept of a basis.

 data Basis v = ...

A basis is a set of elements of a vector space
that span the total. That means that any element of the basis 
can be expanded into a linear sum of basis elements.

 expand :: Basis v -> [(Elem v, Double)]

That allows us to pretty print elements

But the main thing for us is that we can define maps using a basis 
on the domain.  Do I need some kind of associated type ?

-}

data Basis v = Basis [(Elem v, String)]
cannonicalBasisWithNames :: (SingI d, V.Unbox e, Num e) 
                         =>  [String] -> Span d e -> Basis (Span d e)
cannonicalBasisWithNames nms v = 
    let cbwn :: (SingI d, Num e, V.Unbox e) 
              => [String] -> Span d e -> Sing d -> Basis (Span d e)
        cbwn nms v d = 
            let dim :: Int
                dim = fromInteger $ fromSing d
                es = map mke [1 .. dim]
                delta i j = if i == j then 1 else 0
                mke i = EL $ V.generate dim (delta dim)
            in  Basis $ zip es nms
    in withSing (cbwn nms v)

cannonicalBasis :: (SingI d, V.Unbox e, Num e) 
                => Span d e -> Basis (Span d e)
cannonicalBasis v =     
    let cb :: (SingI d, Num e, V.Unbox e) 
            => Span d e -> Sing d -> Basis (Span d e)
        cb v d =
            let dim :: Int
                dim = fromInteger $ fromSing d
                nms = map (("e"++).show) [1 .. dim]
            in  cannonicalBasisWithNames nms v
    in  withSing (cb v)





