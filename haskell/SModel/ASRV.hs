module SModel.ASRV (module SModel.ASRV,
                    module SModel.MixtureModel)
    where

import SModel.MixtureModel
import Probability.Distribution.Gamma     (gamma)
import Probability.Distribution.Transform (logNormal)

gammaRatesDist alpha = gamma alpha (1/alpha)

gammaRatesMedianOn alpha n base = rateMixture base $ gammaRatesMedian alpha n

gammaRatesMedian alpha n = uniformDiscretize (gammaRatesDist alpha) n

gammaRatesOn = gammaRatesMedianOn

gammaRates = gammaRatesMedian
                          
logNormalRatesDist sigmaOverMu = logNormal lmu lsigma where x = log(1+sigmaOverMu^2)
                                                            lmu = -0.5*x
                                                            lsigma = sqrt x

logNormalRatesQuantile logMean logSigma n = uniformDiscretize (logNormal logMean logSigma) n

logNormalRates sigmaOverMu n base = rateMixture base $ logNormalRatesQuantile lmu lsigma n
    where x = log(1+sigmaOverMu^2)
          lmu = -0.5*x
          lsigma = sqrt x

freeRates rateDist base = rateMixture base rateDist
