{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Vector.Unboxed.Base
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors: basic implementation.

module Data.Vector.Unboxed.Base (
  Vector(..), Unbox
  -- MVector(..), IOVector, STVector, 
  -- UnboxViaPrim(..), As(..), IsoUnbox(..)
) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Primitive as P (Vector)
import qualified Data.Vector.Primitive.Mutable as PM (MVector)

import Control.Monad ( liftM )

import Data.Primitive (Prim)
import GHC.Generics
import Data.Coerce
import Data.Kind     (Type)

#define NOT_VECTOR_MODULE
#include "vector.h"

data family MVector s a
data family Vector    a

type instance G.Mutable Vector = MVector

class (G.Vector Vector a, M.MVector MVector a) => Unbox a


-- ---------------
-- Primitive types
-- ---------------

-- | Newtype wrapper which allows to derive unboxed vector in term of
-- primitive vectors using @DerivingVia@ mechanism. This is mostly
-- used as illustration of use of @DerivingVia@ for vector, see examples below.
--
-- First is rather straightforward: we define newtype and use GND to
-- derive 'P.Prim' instance. Newtype instances should be defined
-- manually. Then we use deriving via to define necessary instances.
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia -XMultiParamTypeClasses
-- >>> -- Needed to derive Prim
-- >>> :set -XGeneralizedNewtypeDeriving -XDataKinds -XUnboxedTuples -XPolyKinds
-- >>>
-- >>> import qualified Data.Vector.Unboxed         as U
-- >>> import qualified Data.Vector.Primitive       as P
-- >>> import qualified Data.Vector.Generic         as G
-- >>> import qualified Data.Vector.Generic.Mutable as M
-- >>>
-- >>> newtype Foo = Foo Int deriving P.Prim
-- >>>
-- >>> newtype instance U.MVector s Foo = MV_Int (PM.MVector s Foo)
-- >>> newtype instance U.Vector    Foo = V_Int  (P.Vector    Foo)
-- >>> deriving via (U.UnboxViaPrim Foo) instance M.MVector MVector Foo
-- >>> deriving via (U.UnboxViaPrim Foo) instance G.Vector  Vector  Foo
-- >>> instance Unbox Foo
--
-- Second example is essentially same but with a twist. Instead of
-- using @Prim@ instance of data type, we use underlying instance of @Int@:
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia -XMultiParamTypeClasses
-- >>>
-- >>> import qualified Data.Vector.Unboxed         as U
-- >>> import qualified Data.Vector.Primitive       as P
-- >>> import qualified Data.Vector.Generic         as G
-- >>> import qualified Data.Vector.Generic.Mutable as M
-- >>>
-- >>> newtype Foo = Foo Int
-- >>>
-- >>> newtype instance U.MVector s Foo = MV_Int (PM.MVector s Int)
-- >>> newtype instance U.Vector    Foo = V_Int  (P.Vector    Int)
-- >>> deriving via (U.UnboxViaPrim Int) instance M.MVector MVector Foo
-- >>> deriving via (U.UnboxViaPrim Int) instance G.Vector  Vector  Foo
-- >>> instance Unbox Foo

-- | Isomorphism between type @a@ and its representation in unboxed
-- vector @b@. Default instance coerces between generic
-- representations of @a@ and @b@ which means they have same shape and
-- corresponding fields could be coerced to each other. Note that this
-- means it's possible to have fields that have different types:
--
-- >>> :set -XMultiParamTypeClasses -XDeriveGeneric -XFlexibleInstances
-- >>> import GHC.Generics (Generic)
-- >>> import Data.Monoid
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> :{
-- data Foo a = Foo Int a
--   deriving (Show,Generic)
-- instance VU.IsoUnbox (Foo a) (Int, a)
-- instance VU.IsoUnbox (Foo a) (Sum Int, Product a)
-- :}
--
class IsoUnbox a b where
  -- | Convert value into it representation in unboxed vector.
  toURepr   :: a -> b
  default toURepr :: (Generic a, Generic b, Coercible (Rep a ()) (Rep b ())) => a -> b
  toURepr = to . idU . coerce . idU . from
  -- | Convert value representation in unboxed vector back to value.
  fromURepr :: b -> a
  default fromURepr :: (Generic a, Generic b, Coercible (Rep b ()) (Rep a ())) => b -> a
  fromURepr = to . idU . coerce . idU . from

idU :: f () -> f ()
idU = id


-- | Newtype which allows to derive unbox instances for type @a@ which
-- uses @b@ as underlying representation (usually tuple). Type @a@ and
-- its representation @b@ are connected by type class
-- 'IsoUnbox'. Here's example which uses explicit 'IsoUnbox' instance:
--
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> :{
-- data Foo a = Foo Int a
--   deriving Show
-- instance VU.IsoUnbox (Foo a) (Int,a) where
--   toURepr (Foo i a) = (i,a)
--   fromURepr (i,a) = Foo i a
--   {-# INLINE toURepr #-}
--   {-# INLINE fromURepr #-}
-- newtype instance VU.MVector s (Foo a) = MV_Foo (VU.MVector s (Int, a))
-- newtype instance VU.Vector    (Foo a) = V_Foo  (VU.Vector    (Int, a))
-- deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector MVector (Foo a)
-- deriving via (Foo a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector  Vector  (Foo a)
-- instance VU.Unbox a => VU.Unbox (Foo a)
-- :}
--
--
-- It's also possible to use generic-based instance for 'IsoUnbox'
-- which should work for all product types.
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia -XDeriveGeneric
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> :{
-- data Bar a = Bar Int a
--   deriving (Show,Generic)
-- instance VU.IsoUnbox (Bar a) (Int,a) where
-- newtype instance VU.MVector s (Bar a) = MV_Bar (VU.MVector s (Int, a))
-- newtype instance VU.Vector    (Bar a) = V_Bar  (VU.Vector    (Int, a))
-- deriving via (Bar a `VU.As` (Int, a)) instance VU.Unbox a => VGM.MVector VU.MVector (Bar a)
-- deriving via (Bar a `VU.As` (Int, a)) instance VU.Unbox a => VG.Vector  VU.Vector  (Bar a)
-- instance VU.Unbox a => VU.Unbox (Bar a)
-- :}
--
newtype As (a :: Type) (b :: Type) = As a

newtype instance MVector s (As a b) = MV_UnboxAs (MVector s b)
newtype instance Vector    (As a b) = V_UnboxAs  (Vector b)

instance (IsoUnbox a b, Unbox b) => M.MVector MVector (As a b) where
  -- Methods that just use underlying vector
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  {-# INLINE basicClear #-}
  basicLength      = coerce $ M.basicLength      @MVector @b
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @MVector @b
  basicOverlaps    = coerce $ M.basicOverlaps    @MVector @b
  basicUnsafeNew   = coerce $ M.basicUnsafeNew   @MVector @b
  basicInitialize  = coerce $ M.basicInitialize  @MVector @b
  basicUnsafeCopy  = coerce $ M.basicUnsafeCopy  @MVector @b
  basicUnsafeMove  = coerce $ M.basicUnsafeMove  @MVector @b
  basicUnsafeGrow  = coerce $ M.basicUnsafeGrow  @MVector @b
  basicClear       = coerce $ M.basicClear       @MVector @b
  -- Conversion to/from underlying representation
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n (As x) = MV_UnboxAs <$> M.basicUnsafeReplicate n (toURepr x)
  basicUnsafeRead (MV_UnboxAs v) i = As . fromURepr <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_UnboxAs v) i (As x) = M.basicUnsafeWrite v i (toURepr x)
  basicSet (MV_UnboxAs v) (As x) = M.basicSet v (toURepr x)

instance (IsoUnbox a b, Unbox b) => G.Vector Vector (As a b) where
  -- Method that just use underlying vector
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @b
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @Vector @b
  basicLength       = coerce $ G.basicLength       @Vector @b
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @Vector @b
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @Vector @b
  elemseq _         = seq
  -- Conversion to/from underlying representation
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_UnboxAs v) i = As . fromURepr <$> G.basicUnsafeIndexM v i


#define primMVector(ty,con)                                             \
instance M.MVector MVector ty where {                                   \
  {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicOverlaps #-}                                          \
; {-# INLINE basicUnsafeNew #-}                                         \
; {-# INLINE basicInitialize #-}                                        \
; {-# INLINE basicUnsafeReplicate #-}                                   \
; {-# INLINE basicUnsafeRead #-}                                        \
; {-# INLINE basicUnsafeWrite #-}                                       \
; {-# INLINE basicClear #-}                                             \
; {-# INLINE basicSet #-}                                               \
; {-# INLINE basicUnsafeCopy #-}                                        \
; {-# INLINE basicUnsafeGrow #-}                                        \
; basicLength (con v) = M.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ M.basicUnsafeSlice i n v         \
; basicOverlaps (con v1) (con v2) = M.basicOverlaps v1 v2               \
; basicUnsafeNew n = con `liftM` M.basicUnsafeNew n                     \
; basicInitialize (con v) = M.basicInitialize v                         \
; basicUnsafeReplicate n x = con `liftM` M.basicUnsafeReplicate n x     \
; basicUnsafeRead (con v) i = M.basicUnsafeRead v i                     \
; basicUnsafeWrite (con v) i x = M.basicUnsafeWrite v i x               \
; basicClear (con v) = M.basicClear v                                   \
; basicSet (con v) x = M.basicSet v x                                   \
; basicUnsafeCopy (con v1) (con v2) = M.basicUnsafeCopy v1 v2           \
; basicUnsafeMove (con v1) (con v2) = M.basicUnsafeMove v1 v2           \
; basicUnsafeGrow (con v) n = con `liftM` M.basicUnsafeGrow v n }

#define primVector(ty,con,mcon)                                         \
instance G.Vector Vector ty where {                                     \
  {-# INLINE basicUnsafeFreeze #-}                                      \
; {-# INLINE basicUnsafeThaw #-}                                        \
; {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicUnsafeIndexM #-}                                      \
; {-# INLINE elemseq #-}                                                \
; basicUnsafeFreeze (mcon v) = con `liftM` G.basicUnsafeFreeze v        \
; basicUnsafeThaw (con v) = mcon `liftM` G.basicUnsafeThaw v            \
; basicLength (con v) = G.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ G.basicUnsafeSlice i n v         \
; basicUnsafeIndexM (con v) i = G.basicUnsafeIndexM v i                 \
; basicUnsafeCopy (mcon mv) (con v) = G.basicUnsafeCopy mv v            \
; elemseq _ = seq }

newtype instance MVector s Int = MV_Int (PM.MVector s Int)
newtype instance Vector    Int = V_Int  (P.Vector    Int)
instance Unbox Int
primMVector(Int, MV_Int)
primVector(Int, V_Int, MV_Int)

newtype instance MVector s Float = MV_Float (PM.MVector s Float)
newtype instance Vector    Float = V_Float  (P.Vector    Float)
instance Unbox Float
primMVector(Float, MV_Float)
primVector(Float, V_Float, MV_Float)

newtype instance MVector s Double = MV_Double (PM.MVector s Double)
newtype instance Vector    Double = V_Double  (P.Vector    Double)
instance Unbox Double
primMVector(Double, MV_Double)
primVector(Double, V_Double, MV_Double)

-- ------
-- Tuples
-- ------

#define DEFINE_INSTANCES
#include "unbox-tuple-instances"
