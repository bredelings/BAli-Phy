{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Compiler.Base (seq, error)
import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import Data.OldList (sum)
import Data.Ord ((<), (>))
import Probability.Random (modifiable, makeMCMCState, observe)
import Probability.Distribution.Normal (normal)
import System.IO (putStrLn)
import Numeric.Pow
import MCMC.Moves.Context
import MCMC.Loggers (likelihoodRaw)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Foreign.Vector
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (Unbox(..))
-- Check analytic Normal log densities while changing dependent collections. Lookup tests do not
-- exercise prefix count transfer, obsolete suffixes, or duplicate demands. In a replay-diagnostic
-- build, also run with BALI_PHY_REPLAY_LATE=1 to cover count transfer after old counts are removed.
check expectedMean c = do
  p <- likelihoodRaw c
  let expected = -0.5 * expectedMean * expectedMean - 0.9189385332046727
  if abs (ln p - expected) < 1e-10 then return ()
    else error "dependent collection likelihood changed incorrectly"

-- Exercise unchanged inputs, changed prefixes, shorter suffixes, repeated demands, and rejection.
-- This remains useful while collections record dependent USE/FORCE edges in the interpreter.
main = do
  let control = modifiable (0::Int)
      a = modifiable (1::Double)
      b = modifiable (2::Double)
      d = modifiable (3::Double)
      values = if control < 2 then IM.fromList [(0,a),(1,b),(2,d)]
                              else IM.fromList [(0,d),(1,a)]
      keys = if control < 2 then IS.fromList [0,1,2] else IS.fromList [0,1]
      v = IM.restrictKeysToVector values keys
      tailValue = if b < 0 then error "obsolete suffix demanded" else d
      xs = if control < 2 then a : (if b > 0 then [b,tailValue] else []) else [d,a,a]
      vs = listToVector xs
      uv = basicConcatN 2 [if control < 2 then U.fromList [a] else U.fromList [d],U.fromList [b]]
      f = IM.forceAll values
      -- These references exercise flattened USE-as-FORCE and explicit FORCE edges, which
      -- the direct collection entries above do not cover. Repeated entries test count multiplicity.
      wrapperControl = modifiable (0::Int)
      x = modifiable (10::Double)
      z = modifiable (20::Double)
      y = modifiable (2::Double)
      wrappedUse = x `seq` (1::Double)
      wrappedForce = z `seq` (1::Double)
      wrappedList = if wrapperControl < 2 then [wrappedUse,wrappedUse,y] else []
      wrappedMap = if wrapperControl < 2 then IM.fromList [(0,wrappedForce),(1,wrappedForce)]
                                        else IM.fromList []
      extra = IM.forceAll wrappedMap `seq` sum (vectorToList (listToVector wrappedList))
      result = extra + sum (vectorToList v) + sum (vectorToList vs) + (uv U.! 0) + (uv U.! 1)
  c <- makeMCMCState $ do
    observe (0::Double) (normal (f `seq` result) 1)
    return []
  check 19 c
  setAtomicModifiableValueInContext control 1 c
  check 19 c
  setAtomicModifiableValueInContext a 1.5 c
  check 20.5 c
  setAtomicModifiableValueInContext b 2.5 c
  check 22 c
  setAtomicModifiableValueInContext d 3.5 c
  check 23 c
  proposal <- copyContext c
  setAtomicModifiableValueInContext b (-1) proposal
  check 10 proposal
  releaseContext proposal
  check 23 c
  setAtomicModifiableValueInContext b (-1) c
  check 10 c
  setAtomicModifiableValueInContext b 2.5 c
  check 23 c
  setAtomicModifiableValueInContext control 3 c
  check 21.5 c
  setAtomicModifiableValueInContext control 4 c
  check 21.5 c
  setAtomicModifiableValueInContext a 2 c
  check 23 c
  setAtomicModifiableValueInContext control 0 c
  check 24.5 c
  -- Replay the flattened prefix before a changed USE, then remove and restore its demands.
  -- Retain these checks while the interpreter bypasses single-force references.
  setAtomicModifiableValueInContext y 3 c
  check 25.5 c
  check 25.5 c
  setAtomicModifiableValueInContext wrapperControl 1 c
  check 25.5 c
  proposal2 <- copyContext c
  setAtomicModifiableValueInContext y 4 proposal2
  check 26.5 proposal2
  releaseContext proposal2
  check 25.5 c
  setAtomicModifiableValueInContext wrapperControl 2 c
  check 20.5 c
  setAtomicModifiableValueInContext x 11 c
  setAtomicModifiableValueInContext z 21 c
  check 20.5 c
  setAtomicModifiableValueInContext wrapperControl 0 c
  check 25.5 c
  check 25.5 c
  putStrLn "completed"
