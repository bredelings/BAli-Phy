module Probability.Distribution.Tree.Yule where

import Tree
import Probability.Random
import Probability.Distribution.Uniform
import Probability.Distribution.Exponential
import Probability.Distribution.Tree.Conditioning
import Probability.Distribution.Tree.Modifiable
import Probability.Distribution.Tree.UniformTimeTree (sampleUniformOrderedTree)
import Probability.Distribution.Tree.Util (Rate)
import qualified Data.IntMap as IntMap
import MCMC

-- Validate configuration separately from tree support. The count is observed through the taxa,
-- even when its probability is retained in the density rather than conditioned away.
validateYule :: Int -> Start -> AgeTreatment -> Condition -> ()
validateYule n start age condition
    | n < 1 = error "Yule: Origin requires at least one taxon"
    | otherwise = checkStart `seq` checkAge `seq` checkCount
    where
      checkStart = case start of
          Crown | n < 2 -> error "Yule: Crown requires at least two taxa"
          _ -> ()
      checkAge = case (start, age, condition) of
          (Crown, Flat, _) -> error "Flat age treatment with Crown is not implemented yet."
          (Origin, Flat, Tips _) -> ()
          (Origin, Flat, _) -> error $ "Flat age treatment requires tip-count conditioning. "
                                    ++ "Use Tips n, or specify an age with AtAge."
          _ -> ()
      checkCount = case condition of
          Tips k | k /= n -> error "Yule: Tips n must match the number of taxa"
          _ -> ()

-- Invalid numerical proposals have zero density; only initialization rejects them with an error.
validYuleParameters :: Rate -> AgeTreatment -> Bool
validYuleParameters lambda age = lambda > 0 && not (isInfinite lambda) && case age of
    AtAge t -> t > 0 && not (isInfinite t)
    Flat -> True

-- Densities use counting measure on non-plane labelled topologies and Lebesgue measure on
-- free internal ages. Each ranked labelled history contributes c_n = 2^(n-1)/n!.
-- With zero-age tips, total length is the sum of internal ages plus the root age.
-- Origin adds the stem t-rootAge; Crown fixes the root and has n-2 free ages.
-- For Tips n, cancel the start-age exponential analytically against P(N_t=n).
-- Flat integrates the origin stem and normalizes by integral P(N_t=n) dt = 1/(n*lambda).
yulePrFactors :: Int -> Rate -> Start -> AgeTreatment -> Condition
              -> WithNodeTimes (WithRoots (Tree l)) -> [Log Double]
yulePrFactors n lambda start age condition tree =
    validateYule n start age condition `seq`
    if validTree then [expTo logDensity] else [0]
    where
      r = root tree
      rootAge = nodeTime tree r
      internal = internalNodes tree
      otherAges = sum [nodeTime tree node | node <- internal, node /= r]
      internalAges = sum [nodeTime tree node | node <- internal]
      logC = fromIntegral (n-1) * log 2 - sum [log (fromIntegral k) | k <- [2..n]]
      validTree = validYuleParameters lambda age && numLeaves tree == n && numNodes tree == 2*n-1
               && all (\node -> nodeTime tree node == 0) (leafNodes tree)
               && all (\node -> let t = nodeTime tree node in t >= 0 && not (isInfinite t)) (getNodes tree)
               && all (\node -> nodeDegree tree node == if node == r then 2 else 3) internal
               && all (\node -> case parentNode tree node of
                                   Nothing -> True
                                   Just p -> nodeTime tree p >= nodeTime tree node) (getNodes tree)
               && case (start, age) of
                    (Origin, AtAge t) -> rootAge <= t
                    (Crown, AtAge t) -> rootAge == t
                    _ -> True
      logDensity = case (start, age, condition) of
          (Origin, AtAge t, Tips _) -> logC + fromIntegral (n-1) * (log lambda - log1mexp (-lambda*t))
                                          - lambda*internalAges
          (Crown, AtAge t, Tips _) -> logC - log (fromIntegral (n-1))
                                         + fromIntegral (n-2) * (log lambda - log1mexp (-lambda*t))
                                         - lambda*otherAges
          (Origin, AtAge t, _) -> logC + fromIntegral (n-1) * log lambda - lambda*(t + internalAges)
          (Crown, AtAge t, _) -> logC + fromIntegral (n-2) * log lambda - lambda*(2*t + otherAges)
          (Origin, Flat, Tips _) -> log (fromIntegral n) + logC + fromIntegral (n-1) * log lambda
                                       - lambda*(internalAges + rootAge)

-- Inverse CDF of an exponential conditioned to be below t. expm1/log1p avoid cancellation
-- for small lambda*t. Node ages sorted below are assigned in backward-merger order.
sampleTruncatedYuleAge :: Rate -> Double -> Random Double
sampleTruncatedYuleAge lambda t = do
    u <- sample $ uniform 0 1
    return $ -log1p (u * expm1 (-lambda*t)) / lambda

-- Uniform pairwise mergers give the Yule ranked topology with fixed labelled leaves.
-- Fixed-age branching times are ordered truncated exponentials; with a flat origin prior,
-- backward waiting times have rates n*lambda, ..., 2*lambda. The singleton has age zero.
sampleYule :: [l] -> Rate -> Start -> AgeTreatment -> Random (WithNodeTimes (WithRoots (Tree l)))
sampleYule taxa lambda start age = do
    if validYuleParameters lambda age then return ()
        else error "Yule: initial rate and age must be finite and positive"
    let n = length taxa
    topology <- sampleUniformOrderedTree n
    ages <- if n == 1 then return [] else case (start, age) of
        (Origin, AtAge t) -> sort <$> sequence (replicate (n-1) (sampleTruncatedYuleAge lambda t))
        (Crown, AtAge t) -> do
            times <- sort <$> sequence (replicate (n-2) (sampleTruncatedYuleAge lambda t))
            return (times ++ [t])
        (Origin, Flat) -> do
            waits <- sequence [sample $ exponential (1 / (fromIntegral k * lambda)) | k <- [n,n-1..2]]
            return $ tail $ scanl (+) 0 waits
    let times = IntMap.fromList $ zip [0..] (replicate n 0 ++ ages)
    return $ time_tree (addLabels (zip [0..] taxa) topology) times

-- A crown age is an input expression, possibly a parameter elsewhere in the model, not a new
-- independently modifiable node age. Replace the root entry after modifying the other ages.
yuleModifiableTimeTree :: Start -> AgeTreatment -> (forall a. a -> a)
                      -> WithNodeTimes (WithRoots (Tree l)) -> WithNodeTimes (WithRoots (Tree l))
yuleModifiableTimeTree start age modf tree = case (start, age) of
    (Crown, AtAge t) -> case modifiableTimeTree modf tree of
        WithNodeTimes rooted times -> WithNodeTimes rooted (IntMap.insert (root tree) t times)
    _ -> modifiableTimeTree modf tree

-- Register the density when the tree is used, even if only a deterministic field is accessed.
-- Singleton and two-tip crown trees still carry count probabilities despite having no free ages.
yuleStructure start age value effect = withEffect (unsafePerformIO $ effect raw) raw
    where raw = yuleModifiableTimeTree start age modifiable value

-- Tree dimension is fixed. Existing moves skip non-modifiable node ages.
yuleEffect tree
    | numLeaves tree == 1 = return ()
    | otherwise = do
        addMove 1 $ walkTimeTreeSampleNNIandNodeTimes tree
        return ()

data Yule l = Yule [l] Rate Start AgeTreatment Condition

instance Dist (Yule l) where
    type Result (Yule l) = WithNodeTimes (WithRoots (Tree l))
    distName _ = "Yule"

instance HasPdf (Yule l) where
    densityWithProperties (Yule taxa lambda start age condition) tree =
        (map fromLogDensity $ yulePrFactors (length taxa) lambda start age condition tree, ())

instance HasAnnotatedPdf (Yule l)

instance Sampleable (Yule l) where
    -- TEMPORARY: without Tips, this draws an initializer conditional on the observed count,
    -- not the registered count-retaining law. The tree must not be created/deleted during MCMC:
    -- that requires a separate proposal density and creation/deletion acceptance correction.
    sample dist@(Yule taxa lambda start age condition) =
        let n = length taxa
        in validateYule n start age condition `seq`
           RanDistribution3 dist yuleEffect (yuleStructure start age) (sampleYule taxa lambda start age)

yule :: [l] -> Rate -> Start -> AgeTreatment -> Condition -> Yule l
yule taxa lambda start age condition =
    validateYule (length taxa) start age condition `seq` Yule taxa lambda start age condition
