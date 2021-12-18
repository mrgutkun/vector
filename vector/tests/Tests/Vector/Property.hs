{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Property
  ( CommonContext
  -- , testSanity
  -- , testPolymorphicFunctions
  -- , testTuplyFunctions 
  -- , testOrdFunctions
  , testEnumFunctions
  -- , testMonoidFunctions
  -- , testBoolFunctions
  -- , testNumFunctions
  -- , testDataFunctions
  -- , testUnstream
  ) where

import Boilerplater (testProperties)
import Utilities (TestData, Model, EqTest, P(..), eq, (===>))

import Data.Orphans ()
import qualified Data.Vector.Generic as V

import Test.QuickCheck

import Test.Tasty

import Text.Show.Functions ()


import System.Random       (Random) 

type CommonContext  a v = (VanillaContext a, VectorContext a v)
type VanillaContext a   = ( Eq a , Show a, Arbitrary a, CoArbitrary a
                          , TestData a, Model a ~ a, EqTest a ~ Property)
type VectorContext  a v = ( Eq (v a), Show (v a), Arbitrary (v a), CoArbitrary (v a)
                          , TestData (v a), Model (v a) ~ [a],  EqTest (v a) ~ Property, V.Vector v a)

testEnumFunctions :: forall a v. (CommonContext a v, Enum a, Ord a, Num a, Random a) => v a -> [TestTree]
{-# INLINE testEnumFunctions #-}
testEnumFunctions _ = $(testProperties
  [ 'prop_enumFromN, 'prop_enumFromThenN,
    'prop_enumFromTo, 'prop_enumFromThenTo])
  where
    prop_enumFromN :: P (a -> Int -> v a)
      = (\_ n -> n < 1000)
        ===> V.enumFromN `eq` (\x n -> take n $ scanl (+) x $ repeat 1)

    prop_enumFromThenN :: P (a -> a -> Int -> v a)
      = (\_ _ n -> n < 1000)
        ===> V.enumFromStepN `eq` (\x y n -> take n $ scanl (+) x $ repeat y)

    prop_enumFromTo = \m ->
                      forAll (choose (-2,100)) $ \n ->
                      unP prop m (m+n)
      where
        prop  :: P (a -> a -> v a) = V.enumFromTo `eq` enumFromTo

    prop_enumFromThenTo = \i j ->
                          j /= i ==>
                          forAll (choose (ks i j)) $ \k ->
                          unP prop i j k
      where
        prop :: P (a -> a -> a -> v a) = V.enumFromThenTo `eq` enumFromThenTo

        ks i j | j < i     = (i-d*100, i+d*2)
               | otherwise = (i-d*2, i+d*100)
          where
            d = abs (j-i)
