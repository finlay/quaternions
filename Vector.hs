{- LANGUAGE TypeFamilies #-}
{- LANGUAGE FlexibleInstances #-}
{- LANGUAGE FlexibleContexts #-}
{- LANGUAGE UndecidableInstances #-}
{- LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Vector where

import GHC.TypeLits

import           Data.List (elemIndex)

import qualified Data.Packed.Vector as V
import qualified Numeric.Container as V
import           Foreign.Storable (Storable)


-- Vector space is constructed by spanning over 
-- a list of elements of type a.
-- The scalars are given by the type s.
data Span a s = Span [a]


class VectorSpace (v :: Nat -> s -> *) where
    type Elem v :: k -> Constraint
    zero   :: Elem v
    minus  :: Elem v -> Elem v
    plus   :: Elem v -> Elem v -> Elem v
    scale  :: s -> Elem v -> Elem v
