{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
module Main where

import Compiler.Error (error)
import Compiler.Fractional
import Compiler.Num
import Data.Function ((.))
import Data.List (map)
import Data.Tuple (snd)
import Numeric.Pow (ln, expTo)
import Numeric.ProbDensity (fromLogDensity, collapseDensity, reciprocalDensity)
import Probability.Dist
import Probability.Distribution.RecDist (RecDist(..))
import System.IO (print)

-- Existing likelihood tests cover particular distributions, not the two HasPdf defaults.
-- Check both instance styles, factor preservation, lazy properties, and recursive forwarding.
-- These checks remain useful while HasPdf offers both scalar and joint density methods.
data Simple = Simple
instance Dist Simple where
    type Result Simple = Double
instance HasPdf Simple where
    pdf _ x = expTo x

data Rich = Rich Double
instance Dist Rich where
    type Result Rich = Double
instance HasPdf Rich where
    type DistProperties Rich = Double
    densityWithProperties (Rich label) x =
        ([fromLogDensity (expTo x), reciprocalDensity (fromLogDensity (expTo (x/2)))], label)

main = do
    print (ln (pdf Simple 4), map (ln . collapseDensity) (densities Simple 4))
    print (ln (pdf (Rich 7) 4), map (ln . collapseDensity) (densities (Rich 7) 4),
           snd (densityWithProperties (Rich 7) 4))
    print (ln (pdf (Rich (error "unused properties")) 4))
    let recursive = RecDist (\x -> Rich (x+3))
    print (ln (pdf recursive 4), snd (densityWithProperties recursive 4))
