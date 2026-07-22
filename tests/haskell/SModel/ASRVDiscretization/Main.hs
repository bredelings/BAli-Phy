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
import Data.OldList ((!!), map, tail, unzip, zip, zipWith)
import Data.Ord
import Data.Tuple (fst, snd)
import Probability.Distribution.Discrete (unpackDiscrete)
import SModel.ASRV
import System.IO (print)

near x y = abs (x - y) < 1.0e-11

nearRelative x y = abs (x / y - 1.0) < 1.0e-11

nearPublished x y = abs (x - y) < 5.0e-5

finite x = not (isInfinite x || isNaN x)

validQuadraturePair pair = finite (fst pair) && finite (snd pair) && fst pair > 0 && snd pair >= 0

validMeanPair pair = finite (fst pair) && finite (snd pair) && fst pair >= 0 && snd pair >= 0

ordered xs = all (\pair -> fst pair <= snd pair) $ zip xs (tail xs)

moments distribution = (sum weights, sum $ zipWith (*) rates weights,
                         sum $ zipWith (\rate weight -> rate * rate * weight) rates weights)
    where (rates, weights) = unzip $ unpackDiscrete distribution

-- Check the Gamma discretizations against analytic rules, published values, and limiting cases.
main = do
    let meanRule = gammaRatesMean 1.0 2
        meanPairs = unpackDiscrete meanRule
        meanRates = map fst meanPairs
        (meanWeights, meanRate, _) = moments meanRule
        yangRule = gammaRatesMean 0.5 4
        yangPairs = unpackDiscrete yangRule
        yangRates = map fst yangPairs
        infiniteMeanPairs = unpackDiscrete $ gammaRatesMean (1.0 / 0.0) 4
        smallMeanPairs = unpackDiscrete $ gammaRatesMean 3.4181825842769832e-16 20
        tinyMeanPairs = unpackDiscrete $ gammaRatesMean 1.0e-100 20
        extremeMeanRule = gammaRatesMean 1.0e-300 20
        extremeMeanPairs = unpackDiscrete extremeMeanRule
        (_, extremeCategoryMean, _) = moments extremeMeanRule
        gammaRule = gammaRatesQuadrature 1.0 2
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
        genericPairs = unpackDiscrete $ gammaRates 1.0 2
    print (near meanWeights 1.0
        && near meanRate 1.0
        && near (meanRates !! 0) (1.0 - log 2.0)
        && near (meanRates !! 1) (1.0 + log 2.0)
        && all (\pair -> near (snd pair) 0.25) yangPairs
        && nearPublished (yangRates !! 0) 0.0334
        && nearPublished (yangRates !! 1) 0.2519
        && nearPublished (yangRates !! 2) 0.8203
        && nearPublished (yangRates !! 3) 2.8944
        && ordered yangRates
        && all (\pair -> near (fst pair) 1.0 && near (snd pair) 0.25) infiniteMeanPairs
        && all validMeanPair smallMeanPairs
        && all validMeanPair tinyMeanPairs
        && all validMeanPair extremeMeanPairs
        && near extremeCategoryMean 1.0
        && near gammaWeights 1.0
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
        && near (fst (genericPairs !! 0)) (1.0 - log 2.0)
        && near (fst (genericPairs !! 1)) (1.0 + log 2.0)
        && near (snd (genericPairs !! 0)) 0.5
        && near (snd (genericPairs !! 1)) 0.5)
