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

module Data.Vector.Unboxed (
  -- * Unboxed vectors
  Vector,

  -- * Conversions
  -- ** Lists
  toList, fromList

) where

import Data.Vector.Unboxed.Base (Vector, Unbox)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Bundle as Bundle

#define NOT_VECTOR_MODULE
#include "vector.h"

-- See http://trac.haskell.org/vector/ticket/12
instance (Unbox a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)


instance (Show a, Unbox a) => Show (Vector a) where
  showsPrec = G.showsPrec


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
