module SModel.MultiFrequency where

import Bio.Alphabet
import Foreign.Vector
import SModel.Simple
import SModel.Rate
import Tree
import Markov (CTMC, qExp)

-- The node information (i) is used to construct a node property (n) and an edge property (e).
data MultiFrequency i n e = MultiFrequency Alphabet (EVector Int) Double (NodeId -> i) (i -> n) (i -> e)

instance HasAlphabet (MultiFrequency i n e) where
    getAlphabet (MultiFrequency a _ _ _ _ _) = a

instance HasSMap (MultiFrequency i n e) where
    getSMap (MultiFrequency _ s _ _ _ _) = s

instance RateModel (MultiFrequency i n e) where
    rate (MultiFrequency _ _ r _ _ _) = r

instance Scalable (MultiFrequency i e n) where
    scale x (MultiFrequency a smap rate nodeInfo branchQ nodePi) = MultiFrequency a smap (x*rate) nodeInfo branchQ nodePi

nodeInfo (MultiFrequency _ _ _ f _ _) node      = f node  -- get the node info
nodeProp (MultiFrequency _ _ _ f g _) node      = g $ f $ node  -- get the node property
edgeProp (MultiFrequency _ _ _ f _ h) tree edge = h $ f $ node -- get the node property
    where edge' | towardRoot tree edge = reverseEdge edge
                | otherwise = edge
          node = targetNode tree edge'

-- Does it make sense to have a weighted list of CTMCs, independent of any root frequencies?
-- * If we attached root frequencies to each CTMC, then we would have a model that we could simulate from.
-- * If we attach a SINGLE root frequency vector to each CTMC, then
-- Suppose we have something that works like a

-- Question: How do we attach Rates.gamma to one of these models?
-- * we need to be able to rescale the model to have the specified rates, which means that it need to have a rate.

instance (HasRoot t, HasBranchLengths t, RateModel m, CTMC m) => SimpleSModel t (MultiFrequency i (EVector Double) m) where
    distribution model = [1]
    stateLetters (SModelOnTree _ model _) = getSMap model
    branch_transition_p (SModelOnTree tree model f) b = [qExp $ scale (f * branchLength tree b / rate q) $ q]
        where q = edgeProp model tree b
    componentFrequencies (SModelOnTree tree model _) 0 = nodeProp model (root tree)
    componentFrequencies _ _ = error "MultiFrequency: only 1 component"
