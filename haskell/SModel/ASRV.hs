module SModel.ASRV (module SModel.ASRV,
                    module SModel.MixtureModel)
    where

import SModel.MixtureModel
import Numeric.LinearAlgebra (Vector, overrideVectorSize, toList)
import Probability.Distribution.Gamma     (gamma)
import Probability.Distribution.Transform (logNormal)

foreign import trcall "Distribution:gammaQuadratureNative"
    gammaQuadratureNative :: Double -> Int -> (Vector Double, Vector Double)

foreign import trcall "Distribution:logNormalQuadratureNative"
    logNormalQuadratureNative :: Double -> Double -> Int -> (Vector Double, Vector Double)

-- Convert native quadrature arrays with a known common length to a discrete distribution.
quadratureDiscrete n (nodes, weights) =
    Discrete $ zip (toList $ overrideVectorSize n nodes) (toList $ overrideVectorSize n weights)

gammaRatesDist alpha = gamma alpha (1/alpha)

gammaRatesMedianOn alpha n base = rateMixture base $ gammaRatesMedian alpha n

gammaRatesMedian alpha n = uniformDiscretize (gammaRatesDist alpha) n

gammaRatesQuadrature alpha n = quadratureDiscrete n $ gammaQuadratureNative alpha n

gammaRatesQuadratureOn alpha n base = rateMixture base $ gammaRatesQuadrature alpha n

gammaRatesOn = gammaRatesMedianOn

gammaRates = gammaRatesMedian
                          
logNormalRatesDist sigmaOverMu = logNormal lmu lsigma where x = log(1+sigmaOverMu^2)
                                                            lmu = -0.5*x
                                                            lsigma = sqrt x

logNormalRatesQuantile logMean logSigma n = uniformDiscretize (logNormal logMean logSigma) n

logNormalRatesQuadrature logMean logSigma n = quadratureDiscrete n $ logNormalQuadratureNative logMean logSigma n

logNormalRates sigmaOverMu n base = rateMixture base $ logNormalRatesQuantile lmu lsigma n
    where x = log(1+sigmaOverMu^2)
          lmu = -0.5*x
          lsigma = sqrt x

freeRates rateDist base = rateMixture base rateDist
