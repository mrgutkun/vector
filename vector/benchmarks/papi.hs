{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE TypeApplications         #-}
module Main where

import Algo.MutableSet (mutableSet)
import Algo.ListRank   (listRank)
import Algo.Rootfix    (rootfix)
import Algo.Leaffix    (leaffix)
import Algo.AwShCC     (awshcc)
import Algo.HybCC      (hybcc)
import Algo.Quickhull  (quickhull)
import Algo.Spectral   (spectral)
import Algo.Tridiag    (tridiag)
import Algo.FindIndexR (findIndexR, findIndexR_naive, findIndexR_manual)

import TestData.ParenTree (parenTree)
import TestData.Graph     (randomGraph)

import Control.Monad
import Control.Exception (evaluate)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import Data.Int
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.Random.Stateful
import System.Mem
import Test.Tasty
import Test.Tasty.Providers
import Test.Tasty.Options
import Test.Tasty.Runners
import Text.Printf

----------------------------------------------------------------

foreign import capi "papi.h value PAPI_OK"          papi_OK          :: CInt
foreign import capi "papi.h value PAPI_NULL"        papi_NULL        :: CInt
foreign import capi "papi.h value PAPI_VER_CURRENT" papi_VER_CURRENT :: CInt

foreign import capi "papi.h value PAPI_TOT_INS" papi_TOT_INS :: CInt
foreign import capi "papi.h value PAPI_FP_INS"  papi_FP_INS  :: CInt
foreign import capi "papi.h value PAPI_BR_INS"  papi_BR_INS  :: CInt
foreign import capi "papi.h value PAPI_BR_MSP"  papi_BR_MSP  :: CInt

foreign import capi "papi.h PAPI_library_init"
  papi_library_init :: CInt -> IO CInt

foreign import capi unsafe "papi.h PAPI_create_eventset"
  papi_create_eventset :: Ptr CInt -> IO CInt
foreign import capi unsafe "papi.h PAPI_cleanup_eventset"
  papi_cleanup_eventset :: CInt -> IO CInt
foreign import capi unsafe "papi.h PAPI_destroy_eventset"
  papi_destroy_eventset :: Ptr CInt -> IO CInt

foreign import capi unsafe "papi.h PAPI_add_event"
  papi_add_event :: CInt -> CInt -> IO CInt

foreign import capi unsafe "papi.h PAPI_start"
  papi_start :: CInt -> IO CInt
foreign import capi unsafe "papi.h PAPI_stop"
  papi_stop :: CInt -> Ptr CLLong -> IO CInt
foreign import capi unsafe "papi.h PAPI_read"
  papi_read :: CInt -> Ptr CLLong -> IO CInt
foreign import capi unsafe "papi.h PAPI_reset"
  papi_reset :: CInt -> IO CInt

call :: IO CInt -> IO ()
call f = f >>= \case
  n | n == papi_OK -> pure ()
    | otherwise    -> error $ "PAPI call failed: " ++ show n

----------------------------------------------------------------

newtype Benchmark = Benchmark (IO ())

instance IsTest Benchmark where
  testOptions = pure []
  run opts (Benchmark io) _
    | 1 == getNumThreads (lookupOption opts) = do
        -- FIXME: deal with exceptions
        alloca $ \pEvt -> do
          allocaArray 4 $ \vals -> do
            -- Set up counters
            do n <- papi_library_init papi_VER_CURRENT
               when (n /= papi_VER_CURRENT) $ error "PAPI init failed"
            evt <- do poke pEvt papi_NULL
                      call $ papi_create_eventset pEvt
                      peek pEvt
            call $ papi_add_event evt papi_TOT_INS
            call $ papi_add_event evt papi_FP_INS
            call $ papi_add_event evt papi_BR_INS
            call $ papi_add_event evt papi_BR_MSP
            performMajorGC
            -- Measure
            call $ papi_start evt
            io
            call $ papi_stop evt vals
            -- Report
            n_tot_ins <- fromIntegral @_ @Double <$> peekElemOff vals 0
            n_fp_ins  <- fromIntegral @_ @Double <$> peekElemOff vals 1
            n_br_ins  <- fromIntegral @_ @Double <$> peekElemOff vals 2
            n_br_msp  <- fromIntegral @_ @Double <$> peekElemOff vals 3
            pure $ testPassed $ printf
              -- "TOT_INS=%12i    FP_INS=%12i     BR_INS=%12i     BR_MSP=%12i"
              "TOT_INS=%.3e    FP_INS=%.3e     BR_INS=%.3g     BR_MSP=%.3g  Misprediction=%.2f%%"
              n_tot_ins n_fp_ins n_br_ins n_br_msp (100*n_br_msp/n_br_ins)
    | otherwise = pure $ testFailed
        "Benchmarks must not be run concurrently. Please pass -j1 or use single threaded runtime."


bench :: String -> Benchmark -> TestTree
bench = singleTest

whnf :: (a -> b) -> a -> Benchmark
whnf f a = Benchmark $ do _ <- evaluate (f a)
                          return ()



----------------------------------------------------------------
indexFindThreshold :: Double
indexFindThreshold = 2e-5

main :: IO ()
main = do
  let useSize = 1000000
  gen <- newIOGenM (mkStdGen 42)

  let (!lparens, !rparens) = parenTree useSize
  (!nodes, !edges1, !edges2) <- randomGraph gen useSize

  let randomVector l = U.replicateM l (uniformDoublePositive01M gen)
  !as <- randomVector useSize
  !bs <- randomVector useSize
  !cs <- randomVector useSize
  !ds <- randomVector useSize
  !sp <- randomVector (floor $ sqrt $ fromIntegral useSize)
  -- vi <- MV.new useSize
  -- , bench "tridiag"    $ whnf tridiag (as,bs,cs,ds)
  defaultMain $ testGroup "vector"
    [ bench "listRank"   $ whnf listRank useSize
    , bench "rootfix"    $ whnf rootfix (lparens, rparens)
    , bench "leaffix"    $ whnf leaffix (lparens, rparens)
    , bench "awshcc"     $ whnf awshcc (nodes, edges1, edges2)
    , bench "hybcc"      $ whnf hybcc  (nodes, edges1, edges2)
    , bench "quickhull"  $ whnf quickhull (as,bs)
    , bench "spectral"   $ whnf spectral sp
    , bench "tridiag"    $ whnf tridiag (as,bs,cs,ds)
    -- , bench "mutableSet" $ nfIO $ mutableSet vi
    , bench "findIndexR" $ whnf findIndexR ((<indexFindThreshold), as)
    , bench "findIndexR_naïve" $ whnf findIndexR_naive ((<indexFindThreshold), as)
    , bench "findIndexR_manual" $ whnf findIndexR_manual ((<indexFindThreshold), as)
    , bench "minimumOn"  $ whnf (U.minimumOn (\x -> x*x*x)) as
    , bench "maximumOn"  $ whnf (U.maximumOn (\x -> x*x*x)) as
    ]
