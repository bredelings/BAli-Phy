module SModel.MultiFrequency where

import Bio.Alphabet
import Foreign.Vector
import SModel.Simple
import SModel.Rate
import Tree
import Markov (CTMC, qExp, getEqFreqs)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import SModel.Frequency (frequenciesFromDict)
import SModel.MarkovModulated
import Reversible

-- NOTE: The model here needs to know the list of node names on the tree.
-- * QUESTION: Is it really separate from the tree then?
-- * QUESTION: Suppose we make SimpleSModel operate directly on (SModelOnTree t m)?
-- * QUESTION: Suppose we put the tree into MultiFrequency?
--           - This allows getting the nodes for the node info.
--           - This allows getting the branches to apply q(b) and get an alphabet and smap out...

-- The node information (i) is used to construct a node property (n) and an edge property (e).
data MultiFrequency t i e = MultiFrequency t (NodeId -> i) (i -> e)

nodeFreq (MultiFrequency _ f g) node = getEqFreqs $ g $ f $ node    -- get the node property
edgeRates (MultiFrequency tree f g) edge = g $ f $ node             -- get the node rates
    where edge' | towardRoot tree edge = reverseEdge edge
                | otherwise = edge
          node = targetNode tree edge'

instance (HasRoot t, HasAlphabet e) => HasAlphabet (MultiFrequency t i e) where
    getAlphabet model@(MultiFrequency tree _ _) = getAlphabet (edgeRates model branch)
        where branch = head $ IntSet.elems (getEdgesSet tree)

instance (HasRoot t, HasSMap e) => HasSMap (MultiFrequency t i e) where
    getSMap model@(MultiFrequency tree _ _) = getSMap (edgeRates model branch)
        where branch = head $ IntSet.elems (getEdgesSet tree)

instance Scalable e => Scalable (MultiFrequency t i e) where
    scaleBy x (MultiFrequency tree nodeInfo branchQ) = MultiFrequency tree nodeInfo (scaleBy x . branchQ)

-- All branches need to have the same rate!
instance (HasRoot t, RateModel e) => RateModel (MultiFrequency t i e) where
    rate model@(MultiFrequency tree _ _) = rate (edgeRates model branch)
        where branch = head $ IntSet.elems (getEdgesSet tree)

-- It would be nice if the branchQ and the nodePi contained the _alphabet_ *before* applying them to a value.
-- It would be nice if they also revealed the _smap_ before they are applied to a value.
-- Neither the alphabet nor the smap should depend on any parameter values that determine the rate matrix.

{- NOTE:
   * The parameter 'nodeInfo' has information (such as the GC content, or the frequencies) at a node.
     It is stored as an IntMap from Int node names to the information.
     The Q matrix is a function of this information.

   * The parameter nodePi instead gives the starting frequencies for a node if it is the root node.
     Q: Couldn't we make a branch-heterogeneous model where the heterogeneousness doesn't affect the root frequencies?
     A: Sure, but letting it affect the root frequencies is the more general case.
-}

instance CheckReversible (MultiFrequency t i m) where
    getReversibility _ = NonEq

instance (HasRoot t, HasSMap m, RateModel m, CTMC m, HasBranchLengths t, t ~ t2) => SimpleSModel t (MultiFrequency t2 i m) where
    distribution model = [1]
    stateLetters (SModelOnTree _ model) = getSMap model
    branchTransitionP (SModelOnTree tree model) b = [qExp $ scaleBy (branchLength tree b) $ edgeRates model b]
    componentFrequencies (SModelOnTree tree model) = [nodeFreq model (root tree)]

multiFrequencyUnscaled tree nodeInfo branchQ = MultiFrequency tree (nodeInfo IntMap.!) branchQ

multiFrequency tree nodeInfo branchQ = multiFrequencyUnscaled tree nodeInfo (scaleTo 1 . branchQ)

{-

1. OK, so for homogeneous multi-freq model, we scale the q matrix for each branch to 1 before using it.
   The single component thus has

   q = scaleTo 1 . q'

   And the rate is 1.

2. For rate heterogeneity, we scale each q matrix to rate before using it.
   The component with rate r will have

   q = scaleTo r . q'

   And the rate is r.

3. For the covarion version, we scale the substitution rate to r, but we don't want to scale the switching rate.
   The rate matrix for the component constructed from

   q = tuffleySteel98Unscaled s01 s10 . scaleTo r . q

   And the rate is pi1 * r.

I guess what these all have in common is that every branch has the same rate.
Right now we ensure that by specifying the common rate as part of the model object,
   and rescaling to that rate before we use the rate matrix.
But alternatively, we could move the rate rescaling into the q(pi) function, and compute the rate
   by looking at a particular branch.
This requires knowing the tree, which we don't know in the RateModel instance.  But we do know it
   in the construction function, so we could compute it there and then cache it in the object.
-}

multiFrequencyCovarion tree nodeInfo branchQ rateDist s01 s10 = modelForRate <$> rateDist
    where modelForRate rate = multiFrequencyUnscaled tree nodeInfo (tuffleySteel98Unscaled s01 s10 . scaleTo rate . branchQ)
