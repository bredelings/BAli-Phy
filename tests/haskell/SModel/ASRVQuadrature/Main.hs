{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Classes
import Compiler.Floating
import Compiler.Fractional
import Compiler.Num
import Compiler.RealFloat (isInfinite, isNaN)
import Data.Bool
import Data.Eq
import Data.Foldable (all, sum)
import Data.Function (($))
import Data.OldList ((!!), map, unzip, zipWith)
import Data.Ord
import Data.Tuple (fst, snd)
import Probability.Distribution.Discrete (unpackDiscrete)
import SModel.ASRV
import System.IO (print)

near x y = abs (x - y) < 1.0e-11

nearRelative x y = abs (x / y - 1.0) < 1.0e-11

finite x = not (isInfinite x || isNaN x)

validQuadraturePair pair = finite (fst pair) && finite (snd pair) && fst pair > 0 && snd pair >= 0

moments distribution = (sum weights, sum $ zipWith (*) rates weights,
                         sum $ zipWith (\rate weight -> rate * rate * weight) rates weights)
    where (rates, weights) = unzip $ unpackDiscrete distribution

-- Check analytic two-node rules, moments, and the constant-rate Gamma limit.
main = do
    let gammaRule = gammaRatesQuadrature 1.0 2
        (gammaWeights, gammaMean, gammaSecondMoment) = moments gammaRule
        logNormalRule = logNormalRatesQuadrature 0.0 1.0 2
        logNormalPairs = unpackDiscrete logNormalRule
        infiniteAlphaRule = unpackDiscrete $ gammaRatesQuadrature (1.0 / 0.0) 4
        largeAlphaRule = gammaRatesQuadrature 1.0e100 4
        (largeWeights, largeMean, _) = moments largeAlphaRule
        smallAlpha = 3.4181825842769832e-16
        smallAlphaRule = gammaRatesQuadrature smallAlpha 20
        smallAlphaPairs = unpackDiscrete smallAlphaRule
        (smallWeight, smallMean, smallSecondMoment) = moments smallAlphaRule
        tinyAlpha = 1.0e-100
        tinyAlphaRule = gammaRatesQuadrature tinyAlpha 20
        tinyAlphaPairs = unpackDiscrete tinyAlphaRule
        (tinyWeight, tinyMean, tinySecondMoment) = moments tinyAlphaRule
        extremeAlphaPairs = unpackDiscrete $ gammaRatesQuadrature 1.0e-300 20
        extremeMean = sum $ map (\pair -> fst pair * snd pair) extremeAlphaPairs
        (genericWeights, genericMean, _) = moments $ gammaRates 1.0 2
    print (near gammaWeights 1.0
        && near gammaMean 1.0
        && near gammaSecondMoment 2.0
        && near (fst (logNormalPairs !! 0)) (exp (-1.0))
        && near (snd (logNormalPairs !! 0)) 0.5
        && near (fst (logNormalPairs !! 1)) (exp 1.0)
        && near (snd (logNormalPairs !! 1)) 0.5
        && all (\pair -> near (fst pair) 1.0 && near (snd pair) 0.25) infiniteAlphaRule
        && near largeWeights 1.0
        && near largeMean 1.0
        && all validQuadraturePair smallAlphaPairs
        && near smallWeight 1.0
        && near smallMean 1.0
        && nearRelative smallSecondMoment (1.0 + 1.0 / smallAlpha)
        && all validQuadraturePair tinyAlphaPairs
        && near tinyWeight 1.0
        && near tinyMean 1.0
        && nearRelative tinySecondMoment (1.0 + 1.0 / tinyAlpha)
        && all validQuadraturePair extremeAlphaPairs
        && near extremeMean 1.0
        && near genericWeights 1.0
        && near genericMean 1.0)
