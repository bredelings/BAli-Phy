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
import Data.OldList ((!!), and, length, zipWith)
import Data.Ord
import Numeric.LinearAlgebra (atIndex, flatten, toList)
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

eventFlux distance = equilibriumFlux (\actual -> if actual == distance then 1 else 0)

-- Verify both interpretations of the MNM parameters and the model's
-- Hamming-weighted nucleotide rate.
checkMnm alphabet nucleotideModel v2 v3 =
    [near (e2 / e1) v2,
     near (e3 / e1) v3,
     near (e2 / eventTotal) (v2 / (1 + v2 + v3)),
     near (2 * e2 / nucleotideTotal) (2 * v2 / (1 + 2 * v2 + 3 * v3)),
     near (rate model) (nucleotideTotal / 3)]
  where
    model = mnm alphabet v2 v3 nucleotideModel
    e1 = eventFlux 1 alphabet model
    e2 = eventFlux 2 alphabet model
    e3 = eventFlux 3 alphabet model
    eventTotal = e1 + e2 + e3
    nucleotideTotal = e1 + 2 * e2 + 3 * e3

-- Compare rate matrices while allowing insignificant floating-point differences.
sameMatrix model1 model2 =
    and $ zipWith near (toList $ flatten $ CoreMarkov.getQ model1)
                       (toList $ flatten $ CoreMarkov.getQ model2)

-- Compare the cached triplet rate with direct event- and nucleotide-flux sums.
main = do
    let triplets = mkTriplets dna
        uniformModel = jukes_cantor dna
        oneHitModel = x3 triplets uniformModel
        multihitModel = mnm triplets 0.2 0.03 uniformModel
        eventWeight distance = if distance == 0 then 0 else 1
        nucleotideWeight distance = fromIntegral distance
        oneHitEventRate = equilibriumFlux eventWeight triplets oneHitModel / 3
        oneHitNucleotideRate = equilibriumFlux nucleotideWeight triplets oneHitModel / 3
        multihitNucleotideRate = equilibriumFlux nucleotideWeight triplets multihitModel / 3
        codons = mkCodons dna standard_code
        nonuniformModel = hky85 dna 2 [0.1, 0.2, 0.3, 0.4]
        zeroMnm = mnm codons 0 0 nonuniformModel
        zeroOneHit = x3 codons nonuniformModel

    print [near (rate oneHitModel) oneHitEventRate,
           near oneHitEventRate oneHitNucleotideRate,
           near (rate multihitModel) multihitNucleotideRate]
    print $ checkMnm triplets uniformModel 0.2 0.03
    print $ checkMnm codons nonuniformModel 0.4 0.07
    print [sameMatrix zeroMnm zeroOneHit, near (rate zeroMnm) (rate zeroOneHit)]
