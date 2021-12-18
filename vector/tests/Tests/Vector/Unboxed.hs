{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Unboxed (tests) where

import Data.Data (Data)
import System.Random (Random)

import Test.Tasty (TestTree, testGroup)
import Data.Vector.Unboxed.Base (Vector, Unbox)
import Tests.Vector.Property 
  ( CommonContext )
import Tests.Vector.Property
  ( testEnumFunctions
  )

testNumericUnboxedVector
  :: forall a. ( CommonContext a Vector
               , Unbox a, Ord a, Num a, Enum a, Random a, Data a)
  => Vector a -> [TestTree]
testNumericUnboxedVector dummy = concatMap ($ dummy)
  [
   testEnumFunctions
  ]
 
tests =
  [ testGroup "(Int)" $ 
    testNumericUnboxedVector (undefined :: Vector Int)
  , testGroup "(Float)" $
    testNumericUnboxedVector (undefined :: Vector Float)
  -- , testGroup "(Double)" $
  --   testNumericUnboxedVector (undefined :: Vector Double)
  ]
