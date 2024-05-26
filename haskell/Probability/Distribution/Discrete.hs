module Probability.Distribution.Discrete where

import Probability.Random
import Probability.Distribution.Uniform

-- this is a join
mix fs ds = Discrete [(x, p*f) | (f, d) <- zip' fs ds, (x, p) <- unpackDiscrete d]

unit_mixture x = Discrete [(x, 1)]

-- Should always get its own type?
always = unit_mixture

addComponent ms (x,p) = mix [p, 1-p] [always x, ms]

uniformGrid n = Discrete [( (2*i'+1)/(2*n'), 1/n' ) | i <- take n [0..], let n' = fromIntegral n, let i'=fromIntegral i]

uniformDiscretize dist n = fmap (quantile dist) (uniformGrid n)

nComponents (Discrete ds) = length ds

component (Discrete ds) i = fst (ds !! i)

normalizeMixture unnormalized = Discrete [(x, p/total) | (x, p) <- unnormalized]
    where total = sum $ snd <$> unnormalized

-- See https://dennybritz.com/posts/probability-monads-from-scratch/

-- map from a -> probability
data Discrete a = Discrete [(a, Double)]

unpackDiscrete (Discrete pairs) = pairs

-- Note that we can't handle infinitely long discrete distributions.
-- (i) we'd need to store the weights as Prob
-- (ii) we'd need to store the sum of weights as Prob
-- (iiia) we'd need to allow a single uniform to have infinite precision, or
-- (iiib) we'd need to sample a new uniform for each item.

choose u total ((item,p):rest) | u < total+p  = item
                               | otherwise    = choose u (total+p) rest
choose u total []                           = error $ "choose failed!  total = " ++ show total

instance Dist (Discrete a) where
    type Result (Discrete a) = a
    dist_name _ = "discrete"

instance IOSampleable (Discrete a) where
    sampleIO (Discrete pairs) = do
      u <- sampleIO $ Uniform 0 1
      return $ choose u 0 pairs

instance Eq a => HasPdf (Discrete a) where
    pdf (Discrete pairs) x = sum [ doubleToLogDouble p | (item,p) <- pairs, item == x]

instance Dist1D (Discrete Double) where
    cdf (Discrete pairs) x = sum [ p | (item, p) <- pairs, item < x ]

instance MaybeMean (Discrete Double) where
    maybeMean (Discrete pairs) = Just $ sum [ p * item | (item,p) <- pairs]

instance Mean (Discrete Double)

instance MaybeVariance (Discrete Double) where
    maybeVariance dist@(Discrete pairs) = Just $ sum [ p * (item - m)^2 | (item,p) <- pairs] where m = mean dist

instance Variance (Discrete Double)

instance Dist1D (Discrete Int) where
    cdf (Discrete pairs) x = sum [ p | (item, p) <- pairs, fromIntegral item < x]

instance MaybeMean (Discrete Int) where
    maybeMean (Discrete pairs) = Just $ sum [ p * fromIntegral item | (item,p) <- pairs]

instance Mean (Discrete Int)

instance MaybeVariance (Discrete Int) where
    maybeVariance dist@(Discrete pairs) = Just $ sum [ p * (fromIntegral item - m)^2 | (item,p) <- pairs] where m = mean dist

instance Variance (Discrete Int)

instance Eq a => HasAnnotatedPdf (Discrete a) where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable (Discrete a) where
    sample dist@(Discrete pairs) = do
      u <- sample $ Uniform 0 1
      return $ choose u 0 pairs

discrete pairs = Discrete pairs

instance Functor Discrete where
    fmap f (Discrete pairs) = Discrete [(f x,p) | (x,p) <- pairs]

instance Applicative Discrete where
    pure x = Discrete [(x,1)]

    (Discrete fs) <*> (Discrete xs) = Discrete $ do
       (x, px) <- xs
       (f, pf) <- fs
       return (f x, px * pf)

instance Monad Discrete where
    (Discrete xs) >>= f = Discrete $ do
       (x, px) <- xs
       (y, py) <- unpackDiscrete $ f x
       return (y, px * py)

instance Show a => Show (Discrete a) where
    show (Discrete xs) = "Discrete " ++ show xs
