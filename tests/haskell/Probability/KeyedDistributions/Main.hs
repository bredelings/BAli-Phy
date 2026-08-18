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
import qualified Data.Set as Set
import Probability.Dist (HasPdf(pdf), IOSampleable(sampleIO))
import Probability.Distribution.Dirichlet (dirichletOn, symmetricDirichletOn)
import Probability.Distribution.Discrete (delta)
import Probability.Distribution.List (iidOn)
import Probability.Random (runRandomLazy, sample)
import System.IO (print)

near x y = abs (x - y) < 1.0e-12

-- Check map-valued keyed distributions preserve key/value associations; full-model tests
-- do not exercise asymmetric concentrations. This is obsolete if these APIs stop returning maps.
main = do
  let concentrations = Map.fromList [("B", 2), ("A", 1)]
      probabilities = Map.fromList [("B", 0.75), ("A", 0.25)]
      density = toFloating (pdf (dirichletOn concentrations) probabilities) :: Double
  symmetric <- sampleIO $ symmetricDirichletOn (Set.fromList ["B", "A"]) 1
  independent <- runRandomLazy $ sample $ iidOn (Set.fromList ["B", "A"]) (delta 4)
  print (near density 1.5
      && Map.keys symmetric == ["A", "B"]
      && near (sum $ Map.elems symmetric) 1
      && Map.toAscList independent == [("A", 4), ("B", 4)])
