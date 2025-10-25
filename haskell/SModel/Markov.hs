module SModel.Markov (module SModel.Markov, module SModel.Frequency, module Data.Matrix, getEqFreqs) where

import qualified Markov
import           Markov (CTMC(..), MkReversible (..))
import           SModel.Simple
import           SModel.Rate
import           SModel.Frequency
import           Reversible hiding (CanMakeReversible(..))
import qualified Reversible as R (CanMakeReversible(..))
import           Bio.Alphabet
import           Data.Matrix
import           Tree

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

data Markov = Markov Alphabet (EVector Int) Markov.Markov Double

instance CheckReversible Markov where
    getReversibility (Markov a smap m f) = getReversibility m

instance R.CanMakeReversible Markov where
    setReversibility r (Markov a smap m f) = Markov a smap (setReversibility r m) f

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
markov a smap q pi = Markov a smap rm rate
    where rm = Markov.markov q pi
          rate = getEquilibriumRate a smap (getQ rm) pi

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
markov' a smap q = Markov a smap rm rate
    where rm = Markov.markov' q
          rate = getEquilibriumRate a smap (getQ rm) (getEqFreqs rm)

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

nonEq a rates pi = scaleTo 1 $ markov a smap q pi
    where smap = simpleSMap a
          n = length $ getLetters a
          q = Markov.non_rev_from_list n rates

pairNames ls = [l1 ++ l2 | (l1,l2) <- ls]

allOrderedPairs l = [(x,y) | x <- l, y <- l, x /= y]

orderedLetterPairNames a = pairNames $ allOrderedPairs (getLetters a)

nonEq' a rates' pi' = nonEq a rs pi
    where lPairs = allOrderedPairs (getLetters a)
          rs = if length lPairs == length rates' then
                   [ Markov.getElement rates' (l1++l2) | (l1,l2) <- lPairs]
               else
                   error $ "Expected "++show (length lPairs)++" rates but got "++ show (length rates')++"!"
          pi = toVector $ frequenciesFromDict a pi'

labelledEqFrequencies m = zip (getLetters a) frequencies
    where frequencies = vectorToList $ getEqFreqs m
          a = getAlphabet m

labelledStartFrequencies m = zip (getLetters a) frequencies
    where frequencies = vectorToList $ getStartFreqs m
          a = getAlphabet m
