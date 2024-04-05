module Probability.Distribution.Changepoints where
-- Multiple Changepoint Model

import Probability.Random
import Probability.Distribution.Uniform
import Probability.Distribution.Poisson
import Probability.Distribution.List

data Changepoints d = Changepoints Double (Double,Double) d

instance Dist d => Dist (Changepoints d) where
    type Result (Changepoints d) = [(Result d, Double, Double)]
    dist_name _ = "Changepoints"

instance IOSampleable d => IOSampleable (Changepoints d) where
    sampleIO (Changepoints lambda (start,end) dist) = do
      n <- sampleIO $ poisson lambda
      ps'' <- sampleIO $ iid2 (n+1) dist (uniform start end)
      let ((x1,_):ps') = ps
          ps = sortOn snd ps''
          points = toChangePoints x1 start end ps
      return points

instance Sampleable d => Sampleable (Changepoints d) where
    sample (Changepoints lambda (start,end) dist) = do
      n <- sample $ poisson lambda
      ps'' <- sample $ iid2 (n+1) dist (uniform start end)
      let ((x1,_):ps') = ps
          ps = sortOn snd ps''
          points = toChangePoints x1 start end ps
      return points
    
changepoints lambda (start,end) dist = Changepoints lambda (start,end )dist

toChangePoints value start end [] = [(value,start,end)]
toChangePoints value start end ((value2,x2):ps) = (value,start,x2):toChangePoints value2 x2 end ps

{-
{-
 ok, so pos * x1 + (1-pos) * x2 = 1, and x2/x1 = ratio.  x2 = ratio * x1
 And x1/x2 > 0.
 pos * x1 + (1-pos) * ratio * x1 = 1

x1 = 1 / (pos + (1-pos) * ratio)
s2 = ratio / (pos + (1-pos) * ratio)
-}    
                

type Position = Double
type Ratio    = Double
type Length   = Double

data RegionTree = Constant | Split2 Position Ratio RegionTree RegionTree

flattenChangePoints x left length Constant = [(x, left, left+length)]

flattenChangePoints x left length (Split2 pos ratio region1 region2) =
    flattenChangePoints x1 left length1 region1 ++ flattenChangePoints x2 (left+length1) length2 region2
        where 
          length1 = pos*length
          length2 = length - length1
          x1 = x / (pos + (1-pos) * ratio)
          x2 = ratio * x1


-- OK, so if there L nucleotides, and L-1 places to change,
-- we could check that Poisson(lambda * (L-1)) > 0
-- and we are not done, then we have now have M-1 and N-2 places to change

getChangePoints lambda = do
  count <- sample $ poisson $ lambda  -- Or should this be lambda * length - 1?
  if count == 0 then
      return $ Constant
  else do
    ratio <- sample $ log_laplace 0 1
    pos <- sample $ uniform 0 1
    let lambda1 = pos * lambda
        lambda2 = (1-pos) * lambda
    region1 <- getChangePoints lambda1
    region2 <- getChangePoints lambda2
    return $ Split2 pos ratio region1 region2

-}                                                  
