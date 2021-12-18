{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Vector.Unboxed
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors. The implementation is based on type families
-- and picks an efficient, specialised representation for every element type.
-- For example, unboxed vectors of pairs are represented as pairs of unboxed
-- vectors.
--
-- Implementing unboxed vectors for new data types can be very easy. Here is
-- how the library does this for 'Complex' by simply wrapping vectors of
-- pairs.
--
-- @
-- newtype instance 'MVector' s ('Complex' a) = MV_Complex ('MVector' s (a,a))
-- newtype instance 'Vector'    ('Complex' a) = V_Complex  ('Vector'    (a,a))
--
-- instance ('RealFloat' a, 'Unbox' a) => 'Data.Vector.Generic.Mutable.MVector' 'MVector' ('Complex' a) where
--   {-\# INLINE basicLength \#-}
--   basicLength (MV_Complex v) = 'Data.Vector.Generic.Mutable.basicLength' v
--   ...
--
-- instance ('RealFloat' a, 'Unbox' a) => Data.Vector.Generic.Vector 'Vector' ('Complex' a) where
--   {-\# INLINE basicLength \#-}
--   basicLength (V_Complex v) = Data.Vector.Generic.basicLength v
--   ...
--
-- instance ('RealFloat' a, 'Unbox' a) => 'Unbox' ('Complex' a)
-- @
--
-- For newtypes, defining instances is easier since one could use
-- @GeneralizedNewtypeDeriving@ in order to derive instances for
-- 'Data.Vector.Generic.Vector' and 'Data.Vector.Generic.Mutable.MVector',
-- since they're very cumbersome to write by hand:
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving
-- >>>
-- >>> import qualified Data.Vector.Unboxed         as U
-- >>> import qualified Data.Vector.Generic         as G
-- >>> import qualified Data.Vector.Generic.Mutable as M
-- >>>
-- >>> newtype Foo = Foo Int
-- >>>
-- >>> newtype instance U.MVector s Foo = MV_Int (U.MVector s Int)
-- >>> newtype instance U.Vector    Foo = V_Int  (U.Vector    Int)
-- >>> deriving instance M.MVector MVector Foo
-- >>> deriving instance G.Vector  Vector  Foo
-- >>> instance Unbox Foo

module Data.Vector.Unboxed (
  -- * Unboxed vectors
  Vector(V_UnboxAs),

  -- * Conversions
  -- ** Lists
  toList, fromList

) where

import Data.Vector.Unboxed.Base (Vector(..), Unbox)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Bundle as Bundle

import Prelude hiding ( (++), concat )

import Text.Read      ( Read(..), readListPrecDefault )
import Data.Semigroup ( Semigroup(..) )

import qualified GHC.Exts as Exts (IsList(..))


#define NOT_VECTOR_MODULE
#include "vector.h"

-- See http://trac.haskell.org/vector/ticket/12
instance (Unbox a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

-- See http://trac.haskell.org/vector/ticket/12
instance (Unbox a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Unbox a => Semigroup (Vector a) where
  {-# INLINE (<>) #-}
  (<>) = (++)

  {-# INLINE sconcat #-}
  sconcat = G.concatNE

instance Unbox a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = empty

  {-# INLINE mappend #-}
  mappend = (<>)

  {-# INLINE mconcat #-}
  mconcat = concat

instance (Show a, Unbox a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, Unbox a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Unbox e) => Exts.IsList (Vector e) where
  type Item (Vector e) = e
  fromList = fromList
  fromListN = fromListN
  toList = toList


infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors.
(++) :: Unbox a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list.
concat :: Unbox a => [Vector a] -> Vector a
{-# INLINE concat #-}
concat = G.concat

-- | /O(1)/ The empty vector.
empty :: Unbox a => Vector a
{-# INLINE empty #-}
empty = G.empty

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list.
toList :: Unbox a => Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector.
fromList :: Unbox a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector.
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> VU.fromListN 3 [1,2,3,4,5 :: Int]
-- [1,2,3]
-- >>> VU.fromListN 3 [1 :: Int]
-- [1]
fromListN :: Unbox a => Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN


#define DEFINE_IMMUTABLE
