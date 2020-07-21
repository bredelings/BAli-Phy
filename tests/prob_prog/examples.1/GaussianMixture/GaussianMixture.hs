-- See https://github.com/probmods/webppl/blob/dev/examples/gaussianMixture.wppl
import Probability

makeGaussian dim = do
  means <- replicateM dim (uniform 20.0 300.0)
  stds <- replicateM dim (uniform 5.0 50.0)
  return [normal mean std | (mean,std) <- zip means stds]

model = do
  mixtureWeight <- uniform 0.0 1.0
  gaussian1 <- makeGaussian 2
  gaussian2 <- makeGaussian 2

  let gaussianMixture = do
          c <- bernoulli 0.5
          if c == 1 then
              independent gaussian1
          else
              independent gaussian2

  replicateM 100 gaussianMixture

main = do
  x <- random $ model
  return ["x" %=% x]
