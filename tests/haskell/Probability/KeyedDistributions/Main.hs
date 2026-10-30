{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Classes
import Compiler.Fractional
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Floating.Types (FloatConvert(toFloating))
import Data.Function (($))
import Data.List (sum)
import Data.Ord
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Probability.Dist (HasPdf(pdf), IOSampleable(sampleIO))
import Probability.Distribution.Dirichlet (dirichletOn, symmetricDirichletOn)
import Probability.Distribution.Discrete (delta)
import Probability.Distribution.List (iidOn, iidMap)
import Probability.Distribution.Uniform (uniform)
import Probability.Random (runRandomLazy, sample)
import System.IO (print)

near :: Double -> Double -> Bool
near x y = abs (x - y) < 1.0e-12

-- Check map-valued keyed distributions preserve key/value associations; full-model tests
-- do not exercise asymmetric concentrations. This is obsolete if these APIs stop returning maps.
main = do
  let concentrations = Map.fromList [("B", 2), ("A", 1)]
      probabilities = Map.fromList [("B", 0.75), ("A", 0.25)]
      density = toFloating (pdf (dirichletOn concentrations) probabilities) :: Double
  symmetric <- sampleIO $ symmetricDirichletOn (Set.fromList ["B", "A"]) 1
  independent <- runRandomLazy $ sample $ iidOn (Set.fromList ["B", "A"]) (delta 4)
  -- Wrong domains must have zero density even when their values form a valid simplex.
  -- The existing asymmetric case checks correspondence only when the keys already match.
  print (pdf (dirichletOn concentrations) (Map.fromList [("X",0.25),("Y",0.75)]) == 0
      && pdf (dirichletOn concentrations) (Map.singleton "A" 1) == 0
      && near density 1.5
      -- Keyed IID densities must preserve their domains and multiply every component density.
      -- Sampling-only tests cannot detect omitted factors or positive density on different keys.
      && near (toFloating (pdf (iidOn (Set.fromList ["B","A"]) (uniform 0 2))
                             (Map.fromList [("A",0.5),("B",1.5)]))) 0.25
      && pdf (iidOn (Set.fromList ["A","B"]) (delta 4))
             (Map.fromList [("X",4),("Y",4)]) == 0
      && near (toFloating (pdf (iidMap (IntSet.fromList [7,2]) (uniform 0 2))
                             (IntMap.fromList [(2,0.5),(7,1.5)]))) 0.25
      && pdf (iidMap (IntSet.fromList [2,7]) (delta 4))
             (IntMap.fromList [(2,4),(8,4)]) == 0
      && pdf (iidMap (IntSet.fromList []) (delta 4)) IntMap.empty == 1
      && Map.keys symmetric == ["A", "B"]
      && near (sum $ Map.elems symmetric) 1
      && Map.toAscList independent == [("A", 4), ("B", 4)])
