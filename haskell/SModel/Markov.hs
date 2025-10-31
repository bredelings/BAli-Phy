module SModel.Markov (module SModel.Markov, module SModel.Frequency, module Data.Matrix, getEqFreqs) where

import           Reversible
import qualified Markov
import           Markov (CTMC(..))
import           SModel.Simple
import           SModel.Rate
import           SModel.Frequency
import           Bio.Alphabet
import           Data.Matrix
import           Tree
import           Data.Array

foreign import bpcall "SModel:" getEquilibriumRate :: Alphabet -> EVector Int -> Matrix Double -> EVector Double -> Double

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

-- Fields are: alphabet, smap, q, and cached rate.
-- PROBLEM: caching the rate is not quite right, since there are different rates:
--    DNA rate, AA rate, Codon rate, synonymous rate, etc.
data Markov = Markov Alphabet (EVector Int) Markov.Markov Double

wrapMarkov a smap m = Markov a smap m (getEquilibriumRate a smap (getQ m) (getEqFreqs m))

instance CheckReversible Markov where
    getReversibility (Markov _ _ m _) = getReversibility m

instance CanMakeReversible Markov where
    setReversibility rv (Markov a smap m rate) = Markov a smap (setReversibility rv m) rate

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap Markov where
    getSMap (Markov _ s _ _) = s

instance CTMC Markov where
    qExp (Markov _ _ m _) = qExp m
    getStartFreqs (Markov _ _ m _) = getStartFreqs m
    getEqFreqs (Markov _ _ m _) = getEqFreqs m
    getQ (Markov _ _ m  _) = getQ m

simpleSMap a = toVector [0..(alphabetSize a)-1]

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.

markov a smap q pi = wrapMarkov a smap (Markov.markov q pi)

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
eqMarkov a smap q = wrapMarkov a smap (Markov.eqMarkov q)

eqFlow (Markov _ _ m _) = Markov.eqFlow m

eqFlux (Markov _ _ m _) = Markov.eqFlux m

instance HasAlphabet Markov where
    getAlphabet (Markov a _ _ _) = a

instance HasBranchLengths t => SimpleSModel t Markov where
    branchTransitionP (SModelOnTree tree smodel) b = [qExp $ scaleBy (branchLength tree b) smodel]
    stateLetters (SModelOnTree _ rm) = getSMap rm
    componentFrequencies (SModelOnTree _ smodel) = [getStartFreqs smodel]

instance Scalable Markov where
    scaleBy x (Markov a s rm r) = Markov a s (scaleBy x rm) (x*r)

instance RateModel Markov where
    rate (Markov _ _ _ r) = r

-- A markov model needs a map from state -> letter in order to have a rate!
-- For codon models, we basically use smap = id for nucleotides (then divide by three)
--   and amino acids.
-- If we had a covarion model on codons, then we'd need to first collaps the state to
-- a codon, and then collapse the codons to either (i) amino acids or (ii) codons, and then divide by three.

instance Show Markov where
    show (Markov _ _ m _) = show m

nonEq pi m = scaleTo 1 $ markov (getAlphabet m) (getSMap m) (getQ m) pi

pairNames ls = [l1 ++ l2 | (l1,l2) <- ls]

allOrderedPairs l = [(x,y) | x <- l, y <- l, x /= y]

orderedLetterPairNames a = pairNames $ allOrderedPairs (getLetters a)

nonEq' pi' m = nonEq pi m
    where pi = toVector $ frequenciesFromDict (getAlphabet m) pi'

labelledEqFrequencies m = zip (getLetters a) frequencies
    where frequencies = vectorToList $ getEqFreqs m
          a = getAlphabet m

labelledStartFrequencies m = zip (getLetters a) frequencies
    where frequencies = vectorToList $ getStartFreqs m
          a = getAlphabet m

labelledUpperTriangle alphabet matrix = if n == nrows matrix && n == ncols matrix
                                        then [ ((letters!i) ++ (letters!j), getElem i j matrix) | (i, j) <- Markov.all_pairs [0..n-1]]
                                        else error $ "Expected an "++ show (n,n) ++ "  matrix by got an " ++
                                             show (ncols matrix,nrows matrix) ++" matrix!"
    where letters = listArray' (getLetters alphabet)
          n = length letters

labelledOffDiagonal alphabet matrix = if n == nrows matrix && n == ncols matrix
                                      then [ ((letters!i) ++ (letters!j), getElem i j matrix) | i <- [0..n-1], j <- [0..n-1], i /= j]
                                      else error $ "Expected an "++ show (n,n) ++ "  matrix by got an " ++
                                             show (ncols matrix,nrows matrix) ++" matrix!"
    where letters = listArray' (getLetters alphabet)
          n = length letters

