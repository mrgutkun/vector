{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Vector.Generic
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Generic interface to immutable vectors.

module Data.Vector.Generic (
  -- * Immutable vectors
  Vector(..), Mutable,


  -- -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,


  -- -- * Conversions

  -- -- ** Lists
  toList, fromList,

  -- -- * Fusion support

  -- -- ** Conversion to/from Bundles
  stream,


  -- -- ** Show and Read
  showsPrec,
) where

import           Data.Vector.Generic.Base

import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Generic.New as New
import           Data.Vector.Generic.New ( New )

import qualified Data.Vector.Fusion.Bundle as Bundle
import           Data.Vector.Fusion.Bundle ( Bundle, inplace )
import           Data.Vector.Fusion.Stream.Monadic ( Stream )
import           Data.Vector.Internal.Check

import Control.Monad.ST ( runST )
import Control.Monad.Primitive
import Prelude hiding ( enumFromTo, enumFromThenTo,
                        showsPrec )

#include "vector.h"


-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (Vector v a, Num a) => a -> Int -> v a
{-# INLINE enumFromN #-}
enumFromN x n = enumFromStepN x 1 n

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 2 5 = <1,3,5,7,9>
enumFromStepN :: forall v a. (Vector v a, Num a) => a -> a -> Int -> v a
{-# INLINE enumFromStepN #-}
enumFromStepN x y n = elemseq (undefined :: v a) x
                    $ elemseq (undefined :: v a) y
                    $ unstream
                    $ Bundle.enumFromStepN  x y n

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromN' instead.
enumFromTo :: (Vector v a, Enum a) => a -> a -> v a
{-# INLINE enumFromTo #-}
enumFromTo x y = unstream (Bundle.enumFromTo x y)

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Vector v a, Enum a) => a -> a -> a -> v a
{-# INLINE enumFromThenTo #-}
enumFromThenTo x y z = unstream (Bundle.enumFromThenTo x y z)


-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list.
toList :: Vector v a => v a -> [a]
{-# INLINE toList #-}
toList = Bundle.toList . stream

-- | /O(n)/ Convert a list to a vector.
fromList :: Vector v a => [a] -> v a
{-# INLINE fromList #-}
fromList = unstream . Bundle.fromList


-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafely convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze
  :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = stToPrim . basicUnsafeFreeze

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> v a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy dst src = check Unsafe "length mismatch" (M.length dst == basicLength src)
                   $ (dst `seq` src `seq` stToPrim (basicUnsafeCopy dst src))

-- Conversions to/from Bundles
-- ---------------------------

-- | /O(1)/ Convert a vector to a 'Bundle'.
stream :: Vector v a => v a -> Bundle v a
{-# INLINE_FUSED stream #-}
stream v = Bundle.fromVector v

-- | /O(n)/ Construct a vector from a 'Bundle'.
unstream :: Vector v a => Bundle v a -> v a
{-# INLINE unstream #-}
unstream s = new (New.unstream s)

{-# RULES

"stream/unstream [Vector]" forall s.
  stream (new (New.unstream s)) = s

"New.unstream/stream [Vector]" forall v.
  New.unstream (stream v) = clone v

"clone/new [Vector]" forall p.
  clone (new p) = p

"inplace [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  New.unstream (inplace f g (stream (new m))) = New.transform f g m

"uninplace [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  stream (new (New.transform f g m)) = inplace f g (stream (new m))  #-}


-- Recycling support
-- -----------------

-- | Construct a vector from a monadic initialiser.
new :: Vector v a => New v a -> v a
{-# INLINE_FUSED new #-}
new m = m `seq` runST (unsafeFreeze =<< New.run m)

-- | Convert a vector to an initialiser which, when run, produces a copy of
-- the vector.
clone :: Vector v a => v a -> New v a
{-# INLINE_FUSED clone #-}
clone v = v `seq` New.create (
  do
    mv <- M.new (basicLength v)
    unsafeCopy mv v
    return mv)

-- Show
-- ----

-- | Generic definition of 'Prelude.showsPrec'.
showsPrec :: (Vector v a, Show a) => Int -> v a -> ShowS
{-# INLINE showsPrec #-}
showsPrec _ = shows . toList

