module SModel.MultiFrequency where

import Bio.Alphabet
import Foreign.Vector
import SModel.Simple
import SModel.Rate
import Tree
import Markov (CTMC, qExp)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import SModel.Frequency (frequenciesFromDict)

-- NOTE: The model here needs to know the list of node names on the tree.
-- * QUESTION: Is it really separate from the tree then?
-- * QUESTION: Suppose we make SimpleSModel operate directly on (SModelOnTree t m)?
-- * QUESTION: Suppose we put the tree into MultiFrequency?
--           - This allows getting the nodes for the node info.
--           - This allows getting the branches to apply q(b) and get an alphabet and smap out...

-- The node information (i) is used to construct a node property (n) and an edge property (e).
data MultiFrequency i n e = MultiFrequency Alphabet (EVector Int) Double (NodeId -> i) (i -> n) (i -> e)

instance HasAlphabet (MultiFrequency i n e) where
    getAlphabet (MultiFrequency a _ _ _ _ _) = a

instance HasSMap (MultiFrequency i n e) where
    getSMap (MultiFrequency _ s _ _ _ _) = s

instance RateModel (MultiFrequency i n e) where
    rate (MultiFrequency _ _ r _ _ _) = r

instance Scalable (MultiFrequency i e n) where
    scaleBy x (MultiFrequency a smap rate nodeInfo nodePi branchQ) = MultiFrequency a smap (x*rate) nodeInfo nodePi branchQ

nodeInfo (MultiFrequency _ _ _ f _ _) node      = f node        -- get the node info
nodeFreq (MultiFrequency _ _ _ f g _) node      = g $ f $ node  -- get the node property
edgeRates (MultiFrequency _ _ _ f _ h) tree edge = h $ f $ node  -- get the node property
    where edge' | towardRoot tree edge = reverseEdge edge
                | otherwise = edge
          node = targetNode tree edge'

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


instance (HasRoot t, RateModel m, HasBranchLengths t, j ~ EVector Double, CTMC m) => SimpleSModel t (MultiFrequency i j m) where
    distribution model = [1]
    stateLetters (SModelOnTree _ model) = getSMap model
    branchTransitionP (SModelOnTree tree model) b = [qExp $ scaleBy (branchLength tree b) $ q]
        where q = scaleTo (rate model) $ edgeRates model tree b
    componentFrequencies (SModelOnTree tree model) = [nodeFreq model (root tree)]

multiFrequency tree nodeInfo nodePi branchQ = MultiFrequency alphabet smap 1 (nodeInfo IntMap.!) (toVector . nodePi) branchQ
    where alphabet = getAlphabet (branchQ (nodeInfo IntMap.! node))
          smap = getSMap (branchQ (nodeInfo IntMap.! node))
          node = head $ IntSet.elems (getNodesSet tree)

multiFrequency' tree nodeInfo nodePi branchQ = multiFrequency tree nodeInfo (frequenciesFromDict alphabet . nodePi) branchQ
    where alphabet = getAlphabet (branchQ (nodeInfo IntMap.! node))
          node = head $ IntSet.elems (getNodesSet tree)
