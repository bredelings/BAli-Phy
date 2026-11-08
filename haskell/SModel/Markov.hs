module SModel.Markov (module SModel.Markov, module SModel.Frequency, module Numeric.LinearAlgebra, getEqFreqs) where

import           Reversible
import qualified Markov
import           Markov (CTMC(..))
import           SModel.Simple
import           SModel.Property
import           SModel.Rate
import           SModel.Frequency
import           Bio.Alphabet
import           Numeric.LinearAlgebra hiding ((<>))
import           Numeric.LinearAlgebra.Data (vectorSize)
import           Tree
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as Map

foreign import trcall "SModel:getEquilibriumRate" getEquilibriumRateNative :: Alphabet -> U.Vector Int -> Matrix Double -> Vector Double -> Double

getEquilibriumRate alphabet states rates frequencies =
    getEquilibriumRateNative alphabet states rates frequencies

-- This takes the rate matrix q and adds:
-- * pi -> a cached version of the equilibrium frequencies
-- * t -> a scaling factor
-- * r -> a cached version of the rate at equilibrium
--
-- * the observation model
--   + a -> an alphabet
--   + smap -> mapping from markov states -> alphabet states

-- Now, all of these except (a,smap) are just caches for things that we can compute from q.
-- It is the (a,smap) parts that make this into a SUBSTITUTION model.

-- What the LIKELIHOOD wants is just:
-- * component probabilities
--   - number of components = component_probabilities.size()
-- * mapping from states -> observed letters
--   - number of states = smap(m).size()
-- * for each branch, the probability of transitioning from i->j in component m.
--   That is, transition_p(tree, branch, mixture) = matrix(i,j)
-- * for each component, the root frequencies -> root_frequencies(m)
-- I guess this is a "LikelihoodMixtureModel"
-- Ideally, it would also specify if it is reversible
--   that is, all transition_p matricies in component m satisfy detailed balance with respect to the root frequences in component m
--

-- Fields: observable alphabet, state-to-letter map, state names, process, cached rate, annotations.
-- PROBLEM: caching the rate is not quite right, since there are different rates:
--    DNA rate, AA rate, Codon rate, synonymous rate, etc.
data Markov = Markov Alphabet (U.Vector Int) (V.Vector String) Markov.Markov Double ComponentAnnotations

-- Names identify matrix rows independently of the observable letters emitted by those states.
class HasStateNames m where
    getStateNames :: m -> V.Vector String

instance HasStateNames Markov where
    getStateNames (Markov _ _ names _ _ _) = names

-- Expose names in matrix order, including for expanded hidden-state models.
stateNames :: HasStateNames m => m -> [String]
stateNames = V.toList . getStateNames

wrapMarkov a smap names m = Markov a smap names m (getEquilibriumRate a smap (getQ m) (getEqFreqs m)) emptyComponentAnnotations

instance CheckReversible Markov where
    getReversibility (Markov _ _ _ m _ _) = getReversibility m

instance CanMakeReversible Markov where
    setReversibility rv (Markov a smap names m rate properties) = Markov a smap names (setReversibility rv m) rate properties

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap Markov where
    getSMap (Markov _ s _ _ _ _) = s

instance CTMC Markov where
    qExp (Markov _ _ _ m _ _) = qExp m
    getStartFreqs (Markov _ _ _ m _ _) = getStartFreqs m
    getEqFreqs (Markov _ _ _ m _ _) = getEqFreqs m
    getQ (Markov _ _ _ m _ _) = getQ m

simpleSMap a = U.fromList [0..(alphabetSize a)-1]

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.

markov a smap names q pi = wrapMarkov a smap names (Markov.markov q pi)

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
eqMarkov a smap names q = wrapMarkov a smap names (Markov.eqMarkov q)

-- Mark the core model stationary before wrapping, so its rate uses the stored equilibrium frequencies.
eqMarkovFrom a smap names q pi0 = wrapMarkov a smap names (Markov.eqMarkovFrom q pi0)

eqFlow (Markov _ _ _ m _ _) = Markov.eqFlow m

eqFlux (Markov _ _ _ m _ _) = Markov.eqFlux m

eqRelFlux (Markov _ _ _ m _ _) = Markov.eqRelFlux m

instance HasAlphabet Markov where
    getAlphabet (Markov a _ _ _ _ _) = a

instance HasBranchLengths t => SimpleSModel t Markov where
    branchTransitionP (SModelOnTree tree smodel) b = [qExp $ scaleBy (branchLength tree b) smodel]
    stateLetters (SModelOnTree _ rm) = getSMap rm
    componentFrequencies (SModelOnTree _ smodel) = [getStartFreqs smodel]

instance Scalable Markov where
    scaleBy x (Markov a s names rm r annotations) =
        Markov a s names (scaleBy x rm) (x*r) (scaleComponentAnnotations x annotations)

instance RateModel Markov where
    rate (Markov _ _ _ _ r _) = r

instance HasStateProperties Markov where
    getStatePropertyFunctions (Markov _ _ _ _ _ (ComponentAnnotations properties _)) = properties
    setStateProperty name property (Markov a s names rm r (ComponentAnnotations properties conditions)) =
        Markov a s names rm r (ComponentAnnotations (Map.insert name property properties) conditions)
    nPropertyStates (Markov _ _ _ m _ _) = Markov.getNStates m

instance HasComponentConditions Markov where
    getComponentConditions (Markov _ _ _ _ _ (ComponentAnnotations _ conditions)) = conditions
    setComponentCondition name value (Markov a s names rm r (ComponentAnnotations properties conditions)) =
        Markov a s names rm r (ComponentAnnotations properties (Map.insert name value conditions))

instance HasProperties t Markov where
    getProperties (SModelOnTree _ model) = statePropertyMapToComponentPropertyMap $ getStateProperties model
    getConditions (SModelOnTree _ model) = getComponentConditions model

-- A markov model needs a map from state -> letter in order to have a rate!
-- For codon models, nucleotide differences between codons are counted and the
-- result is divided by three to give the rate per nucleotide site.
-- If we had a covarion model on codons, then we'd need to first collaps the state to
-- a codon, and then collapse the codons to either (i) amino acids or (ii) codons, and then divide by three.

instance Show Markov where
    show (Markov _ _ _ m _ _) = show m

-- Replace the root distribution without changing state identities or construction annotations.
-- Recompute the equilibrium rate as before, then propagate normalization to the retained properties.
nonEq pi m@(Markov a smap names _ _ annotations) =
    scaleTo 1 $ Markov a smap names mCore modelRate annotations
  where
    -- Check the input vector without making the whole model depend on this check.
    -- Workaround: guarding the whole model triggers a runtime reference-chain assertion.
    checkedPi | vectorSize pi == Markov.getNStates m = pi
              | otherwise = error "nonEq: expected one frequency per CTMC state"
    mCore = Markov.markov (getQ m) checkedPi
    modelRate = getEquilibriumRate a smap (getQ mCore) (getEqFreqs mCore)

pairNames ls = [l1 ++ l2 | (l1,l2) <- ls]

allOrderedPairs l = [(x,y) | x <- l, y <- l, x /= y]

orderedLetterPairNames a = pairNames $ allOrderedPairs (getLetters a)

nonEq' pi' m = nonEq pi m
    where pi = fromList $ get_ordered_elements (stateNames m) pi' "state frequencies"

-- Pair every numerical state with its name; never silently truncate an expanded state space.
labelledStateFrequencies m frequencies
    | V.length names /= vectorSize frequencies = error "State names and frequencies have different sizes"
    | otherwise = Map.fromList $ zip (V.toList names) (toList frequencies)
  where names = getStateNames m

labelledEqFrequencies m = labelledStateFrequencies m (getEqFreqs m)

labelledStartFrequencies m = labelledStateFrequencies m (getStartFreqs m)

labelledUpperTriangle alphabet matrix = if n == rows matrix && n == cols matrix
                                        then Map.fromList
                                             [ ((letters V.! i) ++ (letters V.! j), atIndex matrix (i,j))
                                             | (i, j) <- Markov.all_pairs [0..n-1]]
                                        else error $ "Expected an "++ show (n,n) ++ "  matrix by got an " ++
                                             show (cols matrix,rows matrix) ++" matrix!"
    where letters = V.fromList (getLetters alphabet)
          n = V.length letters

labelledOffDiagonal alphabet matrix = if n == rows matrix && n == cols matrix
                                      then Map.fromList
                                           [ ((letters V.! i) ++ (letters V.! j), atIndex matrix (i,j))
                                           | i <- [0..n-1], j <- [0..n-1], i /= j]
                                      else error $ "Expected an "++ show (n,n) ++ "  matrix by got an " ++
                                             show (cols matrix,rows matrix) ++" matrix!"
    where letters = V.fromList (getLetters alphabet)
          n = V.length letters
