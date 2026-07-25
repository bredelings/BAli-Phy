{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Bio.Alphabet
import Compiler.Enum
import Compiler.Fractional
import Compiler.Integral (fromIntegral)
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Foldable (sum)
import Data.Function (($))
import Data.OldList ((!!), length)
import Data.Ord
import Numeric.LinearAlgebra (atIndex, toList)
import qualified Markov as CoreMarkov
import SModel
import System.IO (print)

near x y = abs (x - y) < 1.0e-10

-- Count the nucleotide positions at which two alphabet symbols differ.
hammingDistance [] [] = 0
hammingDistance (x:xs) (y:ys) =
    (if x == y then 0 else 1) + hammingDistance xs ys

-- Sum equilibrium transition flux, weighting each event by the requested
-- function of its Hamming distance.
equilibriumFlux weight alphabet model =
    sum [frequencies !! i * weight distance * atIndex q (i, j)
         | i <- [0..n-1],
           j <- [0..n-1],
           let distance = hammingDistance (letters !! i) (letters !! j)]
  where
    letters = getLetters alphabet
    frequencies = toList $ CoreMarkov.getEqFreqs model
    q = CoreMarkov.getQ model
    n = length letters

-- Compare the cached triplet rate with direct event- and nucleotide-flux sums.
main = do
    let alphabet = mkTriplets dna
        nucleotideModel = jukes_cantor dna
        oneHitModel = x3 alphabet nucleotideModel
        multihitModel = mnm alphabet 0.2 0.03 nucleotideModel
        eventWeight distance = if distance == 0 then 0 else 1
        nucleotideWeight distance = fromIntegral distance
        oneHitEventRate = equilibriumFlux eventWeight alphabet oneHitModel / 3
        oneHitNucleotideRate = equilibriumFlux nucleotideWeight alphabet oneHitModel / 3
        multihitNucleotideRate = equilibriumFlux nucleotideWeight alphabet multihitModel / 3

    print [near (rate oneHitModel) oneHitEventRate,
           near oneHitEventRate oneHitNucleotideRate,
           near (rate multihitModel) multihitNucleotideRate]
