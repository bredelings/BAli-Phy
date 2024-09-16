module Probability.Distribution.Changepoints where
-- Multiple Changepoint Model

import Probability.Random
import Probability.Distribution.Uniform
import Probability.Distribution.Poisson
import Probability.Distribution.Transform (logLaplace)
import Probability.Distribution.List (iid2)

data Changepoints d = Changepoints Double (Double,Double) d

instance Dist d => Dist (Changepoints d) where
    type Result (Changepoints d) = [(Result d, Double, Double)]
    dist_name _ = "Changepoints"

instance IOSampleable d => IOSampleable (Changepoints d) where
    sampleIO (Changepoints lambda (start,end) dist) = do
      n <- sampleIO $ poisson lambda
      ps' <- sortOn snd <$> (sampleIO $ iid2 (n+1) dist (uniform start end))
      let ((y1,x1):ps) = ps'
          f x = start + (x-x1)*(end-start)/(end-x1)
          points = toChangePoints y1 start end $ [ (y, f x) | (y,x) <- ps]
      return points

{- We need to pick the point corresponding to the left/starting value in
   an exchangeable way, like we are picking from a set.  Currently, we
   pick the left-most point, and then need to rescale the remaining points.

   We could pick an integer index at random from the sorted list.  This
   should have the right distribution, since it has a 1/n chance of being
   the last added point, the second-to-the-last-added point, etc.  So
   everything should still be uniformly space. BUT, the integer index
   could go down to probability 0 if the size of the set decreases.
   - could we make (uniform_int 0 n) resample if n goes down, and keep the same
     value otherwise?

   We could pick a point at random from the set if we could operate on sets
   and look at the previous value.  If the input set changes, then we could
   keep the same choice if it remains in the set, and choose another entry
   uniformly at random otherwise.
 -}

instance Sampleable d => Sampleable (Changepoints d) where
    sample (Changepoints lambda (start,end) dist) = do
      n <- sample $ poisson lambda
      ps' <- sortOn snd <$> (sample $ iid2 (n+1) dist (uniform start end))
      let ((y1,x1):ps) = ps'
          f x = start + (x-x1)*(end-start)/(end-x1)
          points = toChangePoints y1 start end $ [ (y, f x) | (y,x) <- ps]
      return points
    
changepoints lambda (start,end) dist = Changepoints lambda (start,end) dist

toChangePoints value start end [] = [(value,start,end)]
toChangePoints value start end ((value2,x2):ps) = (value,start,x2):toChangePoints value2 x2 end ps

------------------------------------------------------------------------
{-
 ok, so pos * x1 + (1-pos) * x2 = 1, and x2/x1 = ratio.  x2 = ratio * x1
 And x1/x2 > 0.
 pos * x1 + (1-pos) * ratio * x1 = 1

x1 = 1 / (pos + (1-pos) * ratio)
s2 = ratio / (pos + (1-pos) * ratio)
-}    

data Changepoints2 d = Changepoints2 Double (Double,Double) d

instance (Dist d, Result d ~ Double) => Dist (Changepoints2 d) where
    type Result (Changepoints2 d) = [(Result d, Double, Double)]
    dist_name _ = "Changepoints2"

instance (Sampleable d, Result d ~ Double) => Sampleable (Changepoints2 d) where
    sample (Changepoints2 lambda (start,end) dist) = do
      x <- sample $ dist
      rtree <- getChangePoints lambda
      return $ flattenChangePoints x start (end - start) rtree

changepoints2 lambda (start,end) dist = Changepoints2 lambda (start,end) dist

type Position  = Double
type RatioType = Double
type Length    = Double

data RegionTree = Constant | Split2 Position RatioType RegionTree RegionTree

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
    ratio <- sample $ logLaplace 0 1
    pos <- sample $ uniform 0 1
    let lambda1 = pos * lambda
        lambda2 = (1-pos) * lambda
    region1 <- getChangePoints lambda1
    region2 <- getChangePoints lambda2
    return $ Split2 pos ratio region1 region2
