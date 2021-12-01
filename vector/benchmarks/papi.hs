{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
module Main (main) where

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
import System.Random.Stateful
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
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

foreign import capi unsafe "papi.h PAPI_add_event"
  papi_add_event :: CInt -> CInt -> IO CInt

foreign import capi unsafe "papi.h PAPI_start"
  papi_start :: CInt -> IO CInt
foreign import capi unsafe "papi.h PAPI_read"
  papi_read :: CInt -> Ptr CLLong -> IO CInt
foreign import capi unsafe "papi.h PAPI_reset"
  papi_reset :: CInt -> IO CInt



----------------------------------------------------------------

main :: IO ()
main = do
  let useSize = 100000
  gen <- newIOGenM (mkStdGen 42)

  let (!lparens, !rparens) = parenTree useSize
  (!nodes, !edges1, !edges2) <- randomGraph gen useSize

  let randomVector l = U.replicateM l (uniformDoublePositive01M gen)
  !as <- randomVector useSize
  !bs <- randomVector useSize
  !cs <- randomVector useSize
  !ds <- randomVector useSize
  !sp <- randomVector (floor $ sqrt $ fromIntegral useSize)
  vi <- MV.new useSize
  -- , bench "tridiag"    $ whnf tridiag (as,bs,cs,ds)

  let n_int = 8
  alloca $ \pEvt -> do
    allocaArray n_int $ \vals -> do
      forM_ [0..n_int-1] $ \i -> pokeElemOff vals i 42
      do n <- papi_library_init papi_VER_CURRENT
         when (n /= papi_VER_CURRENT) $ error "PAPI init failed"
      evt <- do poke pEvt papi_NULL
                call $ papi_create_eventset pEvt
                peek pEvt
      call $ papi_add_event evt papi_TOT_INS
      call $ papi_add_event evt papi_FP_INS
      call $ papi_add_event evt papi_BR_INS
      call $ papi_add_event evt papi_BR_MSP
      -- Measure
      call $ papi_start evt
      _ <- evaluate $ tridiag (as,bs,cs,ds)
      call $ papi_read evt vals
      forM_ [0..n_int-1] $ \i -> print =<< peekElemOff vals i
      return ()


call :: IO CInt -> IO ()
call f = f >>= \case
  n | n == papi_OK -> pure ()
    | otherwise    -> error $ "PAPI call failed: " ++ show n
