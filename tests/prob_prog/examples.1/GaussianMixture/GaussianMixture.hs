-- See https://github.com/probmods/webppl/blob/dev/examples/gaussianMixture.wppl
import Probability

makeGaussian dim = do
  means <- replicateM dim (sample $ uniform 20.0 300.0)
  stds <- replicateM dim (sample $ uniform 5.0 50.0)
  return [normal mean std | (mean,std) <- zip means stds]

model = do
  mixtureWeight <- sample $ uniform 0.0 1.0
  gaussian1 <- makeGaussian 2
  gaussian2 <- makeGaussian 2

  let gaussianMixture = do
          c <- sample $ bernoulli 0.5
          if c == 1 then
              sample $ independent gaussian1
          else
              sample $ independent gaussian2

  x <- replicateM 100 gaussianMixture

  return ["x" %=% x]

main = do
  return model
