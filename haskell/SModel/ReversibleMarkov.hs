module SModel.ReversibleMarkov (module SModel.ReversibleMarkov, module SModel.Frequency) where

import qualified Markov
import           SModel.Simple
import           SModel.Rate
import           SModel.Frequency
import           Bio.Alphabet
import           Data.Matrix
import           Tree
import           SModel.EigenExp
import           SModel.LikelihoodMixtureModel

foreign import bpcall "SModel:get_equilibrium_rate" get_equilibrium_rate :: Alphabet -> EVector Int -> Matrix Double -> EVector Double -> Double
foreign import bpcall "SModel:MatrixExp" mexp :: Matrix Double -> Double -> Matrix Double

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

data ReversibleMarkov = ReversibleMarkov Alphabet (EVector Int) (Matrix Double) (EVector Double) Double Double

qExp (ReversibleMarkov a s q pi t r) = mexp q t

get_smap (ReversibleMarkov a s q pi t r) = s

get_alphabet (ReversibleMarkov a s q pi t r) = a

get_q (ReversibleMarkov _ _ q _ t _) = scaleMatrix t q

get_pi (ReversibleMarkov _ _ _ pi _ _) = pi

frequencies (ReversibleMarkov _ _ _ pi _ _) = pi

simple_smap a = list_to_vector [0..(alphabetSize a)-1]

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversible_markov a smap q pi = ReversibleMarkov a smap q2 pi 1.0 (get_equilibrium_rate a smap q2 pi) where q2 = Markov.fixup_diagonal_rates q

equ a = Markov.equ (alphabetSize a) 1.0

gtr_sym exchange a = Markov.gtr_sym (alphabetSize a) exchange 

gtr a s pi = reversible_markov a (simple_smap a) (s %*% plus_f_matrix pi') pi' where pi' = list_to_vector pi

f81     pi a = gtr a (equ a) pi
jukes_cantor a = gtr a (equ a) (uniform_frequencies a)

gtr' s'    pi a = gtr a (gtr_sym' s'    a) (frequencies_from_dict a pi)

-- es' is a [(String,Double)] here
all_pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

letter_pair_names a = [l1++l2|(l1,l2) <- all_pairs (letters a)]

get_element_exchange []                 x y = error ("No exchangeability specified for '" ++ x ++ "'")
get_element_exchange ((key,value):rest) x y = if key == x || key == y then value else get_element_exchange rest x y

-- factor out code to get gtr exch list
-- maybe put ReversibleFrequency into this file.
-- clean up f1x4 and f3x4?
gtr_sym' es' a = gtr_sym es a where lpairs = all_pairs (letters a)
                                    es :: [Double]
                                    es = if length lpairs == length es' then
                                             [get_element_exchange es' (l1++l2) (l2++l1)| (l1,l2) <- lpairs]
                                         else
                                             error $ "Expected "++show (length lpairs)++" exchangeabilities but got "++ show (length es')++"!"

plus_f   a pi s   = gtr a s pi
plus_fe  a s      = plus_f a (uniform_frequencies a) s
plus_gwf a pi f s = reversible_markov a (simple_smap a) (s %*% plus_gwf_matrix pi' f) pi' where pi' = list_to_vector pi

plus_f'  a pi s   = plus_f a (frequencies_from_dict a pi) s
plus_gwf'  a pi f s = plus_gwf a (frequencies_from_dict a pi) f s

instance SimpleSModel ReversibleMarkov where
    branch_transition_p (SingleBranchLengthModel tree smodel@(ReversibleMarkov _ _ _ _ _ _   )) b = [qExp $ scale (branch_length tree b/r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    weighted_frequency_matrix smodel@(ReversibleMarkov _ _ _ pi _ _) = builtin_weighted_frequency_matrix (list_to_vector [1.0]) (list_to_vector [pi])
    frequency_matrix smodel@(ReversibleMarkov _ _ _ pi _ _) = builtin_frequency_matrix (list_to_vector [pi])
    nBaseModels _ = 1
    stateLetters rm = get_smap rm
    getAlphabet (ReversibleMarkov a _ _ _ _ _) = a
    componentFrequencies smodel@(ReversibleMarkov _ _ _ _ _ _) i = [frequencies smodel]!!i

instance RateModel ReversibleMarkov where
    rate (ReversibleMarkov a s q pi t r) = r
    scale x (ReversibleMarkov a s q pi t r) = ReversibleMarkov a s q pi (x*t) (x*r)

instance Show ReversibleMarkov where
    show q = show $ get_q q

instance BranchLengthTree t => LikelihoodMixtureModel (ReversibleMarkov,t) where
    alphabet (m,_) = getAlphabet m
    numObservedStates m = length $ letters $ alphabet m
    componentProbabilities (m,_) = [1.0]
    numComponents _ = 1
    stateToObservedState ((ReversibleMarkov _ smap _ _ _ _),_) (MixtureIndex 0) = smap
    numStates m component = vector_size $ stateToObservedState m component
    transitionProbabilities (smodel,tree) (MixtureIndex 0) b = qExp $ scale (branch_length tree b/r) smodel where r = rate smodel
    rootFrequencies (m,_) (MixtureIndex 0) = get_pi m
    isReversible m = True
