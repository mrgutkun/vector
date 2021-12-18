{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Vector.Primitive
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Unboxed vectors of primitive types. The use of this module is not
-- recommended except in very special cases. Adaptive unboxed vectors defined
-- in "Data.Vector.Unboxed" are significantly more flexible at no performance
-- cost.

module Data.Vector.Primitive (
  -- * Primitive vectors
  Vector(..)
) where

import qualified Data.Vector.Generic           as G (Mutable, Vector(..))
import           Data.Vector.Primitive.Mutable ( MVector(..) )
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )


import Control.Monad ( liftM )

type role Vector nominal

-- | Unboxed vectors of primitive types.
data Vector a = Vector {-# UNPACK #-} !Int       -- ^ offset
                       {-# UNPACK #-} !Int       -- ^ length
                       {-# UNPACK #-} !ByteArray -- ^ underlying byte array


type instance G.Mutable Vector = MVector

instance Prim a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeByteArray marr

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector i n arr)
    = MVector i n `liftM` unsafeThawByteArray arr

  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector i _ arr) j = return $! indexByteArray arr (i+j)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (Vector j _ src)
    = copyByteArray dst (i*sz) src (j*sz) (n*sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE elemseq #-}
  elemseq _ = seq
