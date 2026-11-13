module Probability.Distribution.Tree.BirthDeath where

import Tree
import Probability.Random
import Probability.Distribution.Uniform
import Probability.Distribution.Exponential
import Probability.Distribution.Tree.Conditioning
import Probability.Distribution.Tree.Modifiable
import Probability.Distribution.Tree.Moves (fixedSizeTimeTreeEffect)
import Probability.Distribution.Tree.UniformTimeTree (sampleUniformOrderedTree)
import qualified Data.IntMap as IntMap

foreign import bpcall "TreeDist:birth_death_log_p1" birthDeathLogP1 :: Double -> Double -> Double -> Double -> Double
foreign import bpcall "TreeDist:birth_death_log_survival"
    birthDeathLogSurvival :: Double -> Double -> Double -> Double -> Double
foreign import bpcall "TreeDist:birth_death_log_q" birthDeathLogQ :: Double -> Double -> Double -> Double -> Double
foreign import bpcall "TreeDist:birth_death_hazard" birthDeathHazard :: Double -> Double -> Double -> Double -> Double
foreign import bpcall "TreeDist:birth_death_inverse_hazard"
    birthDeathInverseHazard :: Double -> Double -> Double -> Double -> Double

-- Numerical proposals outside the rate/age domain have zero density, not configuration errors.
validBirthDeathParameters lambda mu rho age =
    lambda > 0 && not (isInfinite lambda) && mu >= 0 && not (isInfinite mu)
    && rho > 0 && rho <= 1
    && case age of AtAge t -> t > 0 && not (isInfinite t)
                   Flat -> True

-- Reconstructed trees have labelled, non-plane topologies and free internal ages. Their
-- coefficient is c_n=2^(n-1)/n!, and each free internal age contributes lambda*p1(age).
-- Origin adds p1(T), divided by S(T) for survival or p1(T)*q(T)^(n-1) for Tips n.
-- Crown conditions both initial lineages to leave sampled descendants, giving (p1(T)/S(T))^2; conditioning
-- on n instead leaves 1/((n-1)*q(T)^(n-2)). The fixed root has no age-density factor.
-- Flat Origin integrates p1(T) from rootAge to infinity and divides by
-- integral P(N_T=n) dT = qInfinity^n/(lambda*n). Use the proper CDF F=q/qInfinity:
-- the result is c_n*n*(1-F(rootAge))*product(lambda*p1(age)/qInfinity).
-- Keep node factors and edge support separate so local age changes reuse other contributions.
birthDeathPrFactors :: (HasRoot t, HasNodeTimes t)
                    => Int -> Double -> Double -> Double -> Start -> AgeTreatment -> Condition -> t -> [Log Double]
birthDeathPrFactors n lambda mu rho start age condition tree =
    validateTreeConditioning "BirthDeath" n start age condition `seq`
    [require validShape, normalizer, require validRoot]
    ++ map nodeFactor (getNodes tree) ++ map edgeFactor (getNodes tree)
    where
      r = root tree
      rootAge = nodeTime tree r
      validParameters = validBirthDeathParameters lambda mu rho age
      validTime t = t >= 0 && not (isInfinite t)
      validShape = numLeaves tree == n && numNodes tree == 2*n-1
                && all (\node -> nodeDegree tree node == if node == r then 2 else 3) (internalNodes tree)
      validRoot = validTime rootAge && case (start, age) of
          (Origin, AtAge t) -> rootAge <= t
          (Crown, AtAge t) -> rootAge == t
          _ -> True
      logC = fromIntegral (n-1) * log 2 - sum [log (fromIntegral k) | k <- [2..n]]
      normalizer = if validParameters && validTime rootAge then expTo logNormalizer else 0
      -- Avoid 0 * log q for singleton Origin and two-tip Crown trees.
      logQPower k t = if k == 0 then 0 else fromIntegral k * birthDeathLogQ lambda mu rho t
      logNormalizer = logC + case (start, age, condition) of
          (Origin, AtAge t, Unconditioned) -> birthDeathLogP1 lambda mu rho t
          (Origin, AtAge t, Surviving) -> birthDeathLogP1 lambda mu rho t - birthDeathLogSurvival lambda mu rho t
          (Origin, AtAge t, Tips _) -> -logQPower (n-1) t
          (Crown, AtAge t, Tips _) -> -log (fromIntegral (n-1)) - logQPower (n-2) t
          (Crown, AtAge t, _) -> 2 * (birthDeathLogP1 lambda mu rho t - birthDeathLogSurvival lambda mu rho t)
          (Origin, Flat, Tips _) -> log (fromIntegral n) - birthDeathHazard lambda mu rho rootAge
      -- Flat divides each branching factor by qInfinity=K/(K+max(mu-lambda,0)), K=rho*lambda.
      logRate = case age of
          Flat | rho == 1 -> log (max lambda mu)
               | mu > lambda -> log (rho*lambda + (mu-lambda)) - log rho
          _ -> log lambda
      nodeFactor node
          | isLeafNode tree node = require (nodeTime tree node == 0)
          | not (validTime t) = 0
          | not validParameters = 1
          | otherwise = case start of
                Crown | node == r -> 1
                _ -> expTo (logRate + birthDeathLogP1 lambda mu rho t)
          where t = nodeTime tree node
      edgeFactor node = case parentNode tree node of
          Nothing -> 1
          Just p -> require (nodeTime tree p >= nodeTime tree node)

-- Conditional branching ages have CDF F(t)/F(T), with H=-log(1-F). The inverse
-- uses expm1/log1p to retain accuracy for short intervals and near-critical rates.
sampleTruncatedBirthDeathAge lambda mu rho t = do
    u <- sample $ uniform 0 1
    let h = -log1p (u * expm1 (-birthDeathHazard lambda mu rho t))
    return $ birthDeathInverseHazard lambda mu rho h

-- In transformed time H, the Flat Origin ages are the first n-1 order statistics
-- of n unit exponentials: their spacings have rates n,n-1,...,2. Fixed-age laws
-- instead sort independent truncated branching ages. Extinct and unsampled histories are integrated out.
sampleBirthDeath taxa lambda mu rho start age = do
    if validBirthDeathParameters lambda mu rho age then return ()
        else error "BirthDeath: initial rates must be finite, lambda > 0, mu >= 0, 0 < rho <= 1, and age > 0"
    let n = length taxa
    topology <- sampleUniformOrderedTree n
    ages <- if n == 1 then return [] else case (start, age) of
        (Origin, AtAge t) -> sort <$> sequence (replicate (n-1) (sampleTruncatedBirthDeathAge lambda mu rho t))
        (Crown, AtAge t) -> do
            times <- sort <$> sequence (replicate (n-2) (sampleTruncatedBirthDeathAge lambda mu rho t))
            return (times ++ [t])
        (Origin, Flat) -> do
            waits <- sequence [sample $ exponential (1 / fromIntegral k) | k <- [n,n-1..2]]
            return $ map (birthDeathInverseHazard lambda mu rho) $ tail $ scanl (+) 0 waits
    let times = IntMap.fromList $ zip [0..] (replicate n 0 ++ ages)
    return $ time_tree (addLabels (zip [0..] taxa) topology) times

data BirthDeath l = BirthDeath [l] Double Double Double Start AgeTreatment Condition

instance Dist (BirthDeath l) where
    type Result (BirthDeath l) = WithNodeTimes (WithRoots (Tree l))
    distName _ = "BirthDeath"

instance HasPdf (BirthDeath l) where
    densityWithProperties (BirthDeath taxa lambda mu rho start age condition) tree =
        (map fromLogDensity $ birthDeathPrFactors (length taxa) lambda mu rho start age condition tree, ())

instance HasAnnotatedPdf (BirthDeath l)

instance Sampleable (BirthDeath l) where
    -- TEMPORARY: without Tips n, this is an initializer conditional on the supplied count.
    -- Creating/deleting the tree during MCMC needs a separate proposal density and correction.
    sample dist@(BirthDeath taxa lambda mu rho start age condition) =
        validateTreeConditioning "BirthDeath" (length taxa) start age condition `seq`
        RanDistribution3 dist fixedSizeTimeTreeEffect (conditionedTimeTreeStructure start age)
                         (sampleBirthDeath taxa lambda mu rho start age)

birthDeath :: [l] -> Double -> Double -> Double -> Start -> AgeTreatment -> Condition -> BirthDeath l
birthDeath taxa lambda mu rho start age condition =
    validateTreeConditioning "BirthDeath" (length taxa) start age condition `seq`
    BirthDeath taxa lambda mu rho start age condition
