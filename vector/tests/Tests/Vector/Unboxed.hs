{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Unboxed (tests) where

import Data.Data (Data)
import System.Random (Random)

import Test.Tasty (TestTree, testGroup)
import Data.Vector.Unboxed.Base (Vector, Unbox)
import Tests.Vector.Property 
  ( CommonContext )
import Tests.Vector.Property
  ( testUnstream
  , testEnumFunctions, testNumFunctions, testBoolFunctions
  )


testBoolUnboxedVector dummy = concatMap ($ dummy)
  [
   testBoolFunctions
  ]

testNumericUnboxedVector
  :: forall a. ( CommonContext a Vector
               , Unbox a, Ord a, Num a, Enum a, Random a, Data a)
  => Vector a -> [TestTree]
testNumericUnboxedVector dummy = concatMap ($ dummy)
  [
    testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "(Bool)" $
    testBoolUnboxedVector (undefined :: Vector Bool)
  , testGroup "(Int)" $
    testNumericUnboxedVector (undefined :: Vector Int)
  , testGroup "(Float)" $
    testNumericUnboxedVector (undefined :: Vector Float)
  , testGroup "(Double)" $
    testNumericUnboxedVector (undefined :: Vector Double)
  , testGroup "unstream" $ testUnstream (undefined :: Vector Int)
  ]
