module SModel.MultiFrequency where

import Bio.Alphabet
import Foreign.Vector
import SModel.Simple
import SModel.Rate
import Tree
import Markov (CTMC, qExp)

-- The node information (i) is used to construct a node property (np) and an edge property (ep).
data MultiFrequency i ep np = MultiFrequency Alphabet (EVector Int) Double (NodeId -> i) (i -> np) (i -> ep)

instance HasAlphabet (MultiFrequency i e n) where
    getAlphabet (MultiFrequency a _ _ _ _ _) = a

instance HasSMap (MultiFrequency i e n) where
    getSMap (MultiFrequency _ s _ _ _ _) = s

instance RateModel (MultiFrequency i e n) where
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

instance (RateModel m, CTMC m) => SimpleSModel (MultiFrequency i m (EVector Double)) where
    distribution model = [1]
    stateLetters model = getSMap model
    branch_transition_p (SingleBranchLengthModel tree model factor) b = [qExp $ scale (factor * branchLength tree b / rate q) $ q]
        where q = edgeProp model tree b
    componentFrequencies model 0 = nodeProp model 0 -- (root tree)
    componentFrequencies _ _ = error "MultiFrequency: only 1 component"
