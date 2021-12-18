{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Unboxed (tests) where

import Data.Data (Data)
import System.Random (Random)

import Test.Tasty (TestTree, testGroup)
import Data.Vector.Unboxed.Base (Vector, Unbox)
import Tests.Vector.Property 
  ( CommonContext )
import Tests.Vector.Property
  ( testUnstream, testSanity
  , testEnumFunctions, testNumFunctions, testDataFunctions, testBoolFunctions
  , testTuplyFunctions, testMonoidFunctions, testOrdFunctions, testPolymorphicFunctions
  )



testGeneralUnboxedVector
  :: forall a. (CommonContext a Vector, Unbox a, Ord a, Data a)
  => Vector a -> [TestTree]
testGeneralUnboxedVector dummy = concatMap ($ dummy)
  [
    testSanity
  -- , testPolymorphicFunctions
  , testOrdFunctions
  , testTuplyFunctions
  , testMonoidFunctions
  , testDataFunctions
  ]

testUnitUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

testBoolUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testBoolFunctions
  ]

testNumericUnboxedVector
  :: forall a. ( CommonContext a Vector
               , Unbox a, Ord a, Num a, Enum a, Random a, Data a)
  => Vector a -> [TestTree]
testNumericUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

testTupleUnboxedVector
  :: forall a. ( CommonContext a Vector
               , Unbox a, Ord a, Data a) => Vector a -> [TestTree]
testTupleUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

tests =
  [ testGroup "()" $
    testUnitUnboxedVector (undefined :: Vector ())
  , testGroup "(Bool)" $
    testBoolUnboxedVector (undefined :: Vector Bool)
  , testGroup "(Int)" $
    testNumericUnboxedVector (undefined :: Vector Int)
  , testGroup "(Float)" $
    testNumericUnboxedVector (undefined :: Vector Float)
  , testGroup "(Double)" $
    testNumericUnboxedVector (undefined :: Vector Double)
  , testGroup "(Int,Bool)" $
    testTupleUnboxedVector (undefined :: Vector (Int, Bool))
  , testGroup "(Int,Bool,Int)" $
    testTupleUnboxedVector
      (undefined :: Vector (Int, Bool, Int))
  , testGroup "unstream" $ testUnstream (undefined :: Vector Int)
  ]
