module SModel.ASRV (module SModel.ASRV,
                    module SModel.MixtureModel)
    where

import SModel.MixtureModel
import Probability.Distribution.Gamma     (gamma)
import Probability.Distribution.Transform (logNormal)

gammaRatesDist alpha = gamma alpha (1/alpha)

gammaRatesOn alpha n base = rateMixture base $ gammaRates alpha n

-- This uses quantiles, so perhaps we should call it gammaRatesQuantile?
gammaRates alpha n = uniformDiscretize (gammaRatesDist alpha) n
                          
logNormalRatesDist sigmaOverMu = logNormal lmu lsigma where x = log(1+sigmaOverMu^2)
                                                            lmu = -0.5*x
                                                            lsigma = sqrt x

logNormalRates sigmaOverMu n base = rateMixture base $ uniformDiscretize (logNormalRatesDist sigmaOverMu) n

freeRates rateDist base = rateMixture base rateDist
