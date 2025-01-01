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


-- QUESTION: Is it really separate from the tree then?
-- QUESTION: Suppose we make SimpleSModel operate directly on (SModelOnTree t m)?
-- QUESTION: Suppose we put the tree into MultiFrequency?
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

instance (HasRoot t, RateModel m, HasBranchLengths t, j ~ EVector Double, CTMC m) => SimpleSModel t (MultiFrequency i j m) where
    distribution model = [1]
    stateLetters (SModelOnTree _ model) = getSMap model
    branchTransitionP (SModelOnTree tree model) b = [qExp $ scaleBy (branchLength tree b) $ q]
        where q = scaleTo (rate model) $ edgeProp model tree b
    componentFrequencies (SModelOnTree tree model) = [nodeProp model (root tree)]

-- It would be nice if the branchQ and the nodePi contained the alphabet before applying them to a value.                                                     
multiFrequency' tree nodeMap nodePi branchQ = MultiFrequency alphabet smap 1 (nodeMap IntMap.!) (toVector . frequenciesFromDict alphabet . nodePi) branchQ
    where alphabet = getAlphabet (branchQ (nodeMap IntMap.! node))
          smap = getSMap (branchQ (nodeMap IntMap.! node))
          node = head $ IntSet.elems (getNodesSet tree)

multiFrequency tree nodeMap nodePi branchQ = MultiFrequency alphabet smap 1 (nodeMap IntMap.!) (toVector . nodePi) branchQ
    where alphabet = getAlphabet (branchQ (nodeMap IntMap.! node))
          smap = getSMap (branchQ (nodeMap IntMap.! node))
          node = head $ IntSet.elems (getNodesSet tree)
