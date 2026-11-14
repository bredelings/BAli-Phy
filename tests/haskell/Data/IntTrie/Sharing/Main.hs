{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Compiler.Base (seq, error)
import Compiler.Num
import Compiler.Fractional
import Compiler.Integral (fromIntegral)
import Control.Monad (return)
import Data.Function (($))
import Data.Functor (fmap)
import Data.Eq
import Data.Ord
import Data.IORef
import Data.IntTrie
import Data.List (sum)
import qualified Data.IntMap as IM
import Probability.Random (modifiable, makeMCMCState, observe)
import Probability.Distribution.Normal (normal)
import MCMC.Moves.Context
import MCMC.Loggers (likelihoodRaw)
import Numeric.Pow
import System.IO (putStrLn)
import Compiler.IO (unsafePerformIO)

-- Count first demands of entries, not arithmetic reevaluations. The counting action stays cached
-- when only the value changes. This deliberately impure instrumentation detects recreated entries.
counted counter value = unsafePerformIO $ do
  value `seq` modifyIORef' counter (+1)
  return value

-- Check both the probability and reuse across contexts; retaining stale cached values must fail too.
check counter expectedCount expectedMean context = do
  probability <- likelihoodRaw context
  let expected = -0.5 * expectedMean * expectedMean - 0.9189385332046727
  if abs (ln probability - expected) < 1e-10 then return () else error "stale trie value"
  count <- readIORef counter
  if count == expectedCount then return () else error "trie computation was recreated"

-- Rebuild the finite map but retain the trie. The tests for IntMap itself do not promise this
-- sharing; retain this case while IntTrie is intended to share work across changing key sets.
main = do
  counter <- newIORef (0::Int)
  let extended = modifiable (0::Int)
      x = modifiable (1::Double)
      source = if extended == 0 then IM.fromList [(0,x),(1,2)] else IM.fromList [(0,x),(1,2),(2,3)]
      trie = fmap (\key -> counted counter (source IM.! fromIntegral key)) (identity::IntTrie Integer)
      values = IM.fromSet (\key -> apply trie (fromIntegral key::Integer)) (IM.keysSet source)
  context <- makeMCMCState $ do
    observe (0::Double) (normal (sum (IM.elems values)) 1)
    return []
  check counter 2 3 context
  setAtomicModifiableValueInContext extended 1 context
  check counter 3 6 context
  setAtomicModifiableValueInContext x 1.5 context
  check counter 3 6.5 context
  proposal <- copyContext context
  setAtomicModifiableValueInContext extended 0 proposal
  setAtomicModifiableValueInContext x 4 proposal
  check counter 3 6 proposal
  releaseContext proposal
  check counter 3 6.5 context
  putStrLn "completed"
