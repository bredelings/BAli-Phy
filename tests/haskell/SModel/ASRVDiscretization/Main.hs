{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Classes
import Compiler.Enum
import Compiler.Floating
import Compiler.Fractional
import Compiler.Num
import Compiler.RealFloat (isInfinite, isNaN)
import Data.Bool
import Data.Eq
import Data.Foldable (all, sum, product)
import Data.Function (($))
import Data.OldList ((!!), (++), map, tail, unzip, zip, zipWith, length, reverse)
import Data.Ord
import Data.Tuple (fst, snd)
import Probability.Distribution.Discrete (unpackDiscrete)
import SModel.ASRV
import SModel.PosSelection (betaQuadratureNative, m7OmegaDist, m8OmegaDist, m8aOmegaDist, m8aTestOmegaDist)
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

betaPairs a b n = unpackDiscrete $ quadratureDiscrete n $ betaQuadratureNative a b n

close tolerance x y = finite x && finite y && abs (x-y) < tolerance

relative tolerance x y = finite x && finite y && abs (x/y-1) < tolerance

weighted f pairs = sum $ map (\pair -> snd pair * f (fst pair)) pairs

betaMoment a b k = product $ map (\j -> (a+j)/(a+b+j)) [0..k-1]

rawMoment k = weighted (\x -> product $ map (\_ -> x) [1..k])

-- Compare whole rules, including their lengths, so missing or reordered mixture categories fail.
samePairs tolerance xs ys = length xs == length ys
    && all (\pair -> close tolerance (fst (fst pair)) (fst (snd pair))
                 && close tolerance (snd (fst pair)) (snd (snd pair))) (zip xs ys)

-- Protect weighted beta moments and tail mass that full-program tests cannot resolve. This group
-- becomes obsolete if M7 stops using finite beta quadrature; absolute-only checks miss lost tails.
betaChecks =
    samePairs 1e-12 (betaPairs 1 1 1) [(0.5,1)]
    && samePairs 1e-12 (betaPairs 1 1 2) [((1-1/sqrt 3)/2,0.5),((1+1/sqrt 3)/2,0.5)]
    && all (\ab -> checkMoments (close 1e-10) (fst ab) (snd ab))
           [(0.352,0.548),(0.404,0.750),(6.6,9.9)]
    && checkMoments (relative 1e-8) 1e-100 1
    && valid bothSmall && close 1e-10 (snd (bothSmall !! 0)) 0.5
    && close 1e-10 (snd (bothSmall !! 3)) 0.5
    && relative 1e-8 (weighted (\x -> x*x*(1-x)*(1-x)) bothSmall) (1e-100/12)
    && all (\a -> valid (betaPairs a a 4) && close 1e-12 (rawMoment 1 (betaPairs a a 4)) 0.5)
           [1e100,1e308]
    && all checkSwitch [0.999e-4,1.001e-4]
    && all (\a -> unavailable (betaPairs a 1 4) && unavailable (betaPairs 1 a 4)) [0,0/0,1/0]
    && samePairs 1e-12 m7Pairs (betaPairs 6.6 9.9 4)
    && all (\g -> samePairs 1e-12 (unpackDiscrete $ m7OmegaDist 0.4 g 4)
                           [(0.4,0.25),(0.4,0.25),(0.4,0.25),(0.4,0.25)]) [0,1e-320]
    && samePairs 1e-12 m8Pairs (scaledBeta ++ [(2,0.1)])
    && samePairs 1e-12 m8aPairs (scaledBeta ++ [(1,0.1)])
    && samePairs 1e-12 (unpackDiscrete $ m8aTestOmegaDist 0.4 0.2 4 0.1 2 0) m8aPairs
    && samePairs 1e-12 (unpackDiscrete $ m8aTestOmegaDist 0.4 0.2 4 0.1 2 1) m8Pairs
  where
    bothSmall = betaPairs 1e-100 1e-100 4
    m7Pairs = unpackDiscrete $ m7OmegaDist 0.4 0.2 4
    m8Pairs = unpackDiscrete $ m8OmegaDist 0.4 0.2 4 0.1 2
    m8aPairs = unpackDiscrete $ m8aOmegaDist 0.4 0.2 4 0.1
    scaledBeta = map (\pair -> (fst pair, 0.9*snd pair)) m7Pairs
    valid pairs = length pairs == 4 && all validMeanPair pairs
        && all (\pair -> fst pair <= 1) pairs && ordered (map fst pairs)
        && close 1e-12 (sum $ map snd pairs) 1
    unavailable pairs = length pairs == 4 && all (\pair -> isNaN (fst pair) && near (snd pair) 0.25) pairs
    checkMoments compare a b = valid (betaPairs a b 4)
        && all (\k -> compare (rawMoment k (betaPairs a b 4)) (betaMoment a b k)) [0..7]
    -- Exercise both sides of the endpoint switch and the reflected rule's pairing.
    checkSwitch a = checkMoments (relative 1e-7) a 1
        && samePairs 1e-7 (reverse $ betaPairs 1 a 4)
               (map (\pair -> (1-fst pair,snd pair)) (betaPairs a 1 4))

-- Check analytic and published rules, genuine point-mass limits, and NaN propagation for unavailable
-- rules; the last case guards against silently substituting a different rate model.
main = do
    let meanRule = gammaRatesMean 1.0 2
        meanPairs = unpackDiscrete meanRule
        meanRates = map fst meanPairs
        (meanWeights, meanRate, _) = moments meanRule
        yangRule = gammaRatesMean 0.5 4
        yangPairs = unpackDiscrete yangRule
        yangRates = map fst yangPairs
        infiniteMeanPairs = unpackDiscrete $ gammaRatesMean (1.0 / 0.0) 4
        zeroMeanPairs = unpackDiscrete $ gammaRatesMean 0.0 4
        nanMeanPairs = unpackDiscrete $ gammaRatesMean (0.0 / 0.0) 4
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
        zeroAlphaRule = unpackDiscrete $ gammaRatesQuadrature 0.0 4
        nanAlphaRule = unpackDiscrete $ gammaRatesQuadrature (0.0 / 0.0) 4
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
        unrepresentableLogNormalPairs = unpackDiscrete $ logNormalRatesQuadrature (-1.0 / 0.0) 0.0 2
        genericPairs = unpackDiscrete $ gammaRates 1.0 2
    print (betaChecks && near meanWeights 1.0
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
        && all (\pair -> isNaN (fst pair) && near (snd pair) 0.25) zeroMeanPairs
        && all (\pair -> isNaN (fst pair) && near (snd pair) 0.25) nanMeanPairs
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
        && all (\pair -> isNaN (fst pair) && near (snd pair) 0.25) zeroAlphaRule
        && all (\pair -> isNaN (fst pair) && near (snd pair) 0.25) nanAlphaRule
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
        && all (\pair -> isNaN (fst pair) && near (snd pair) 0.5) unrepresentableLogNormalPairs
        && near (fst (genericPairs !! 0)) (1.0 - log 2.0)
        && near (fst (genericPairs !! 1)) (1.0 + log 2.0)
        && near (snd (genericPairs !! 0)) 0.5
        && near (snd (genericPairs !! 1)) 0.5)
