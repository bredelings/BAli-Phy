module Forest (module Forest, module Graph) where

import Graph
import Data.Foldable
import Data.Array
import Data.List (lookup)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as T
import Control.DeepSeq

class IsGraph f => IsForest f where
    type family Unrooted f
    type family Rooted f

    unroot :: f -> Unrooted f
    makeRooted :: f -> Rooted f

data Forest = Forest Graph

instance NFData Forest where
    rnf (Forest g) = rnf g

instance IsForest Forest where
    type instance Unrooted Forest = Forest
    type instance Rooted Forest = WithRoots Forest

    unroot f = f
    makeRooted f = addRoots roots f where roots = error "Implement finding connected components!"

instance IsGraph Forest where
    getNodesSet (Forest g) = getNodesSet g
    getEdgesSet (Forest g) = getEdgesSet g

    edgesOutOfNodeSet (Forest g) nodeId = edgesOutOfNodeSet g nodeId
    sourceNode (Forest g) edge = sourceNode g edge
    targetNode (Forest g) edge = targetNode g edge

    getNodeAttributes (Forest g) node = getNodeAttributes g node
    getEdgeAttributes (Forest g) edge = getEdgeAttributes g edge
    getAttributes (Forest g) = getAttributes g

instance IsForest f => IsForest (WithLabels f l) where
    type Unrooted (WithLabels f l) = WithLabels (Unrooted f) l
    type Rooted (WithLabels f l) = WithLabels (Rooted f) l

    unroot (WithLabels t labels) = WithLabels (unroot t) labels
    makeRooted (WithLabels t labels) = WithLabels (makeRooted t) labels

instance IsForest f => IsForest (WithBranchLengths f) where
    type Unrooted (WithBranchLengths f) = WithBranchLengths (Unrooted f)
    type Rooted (WithBranchLengths f) = WithBranchLengths (Rooted f)

    unroot (WithBranchLengths t lengths) = WithBranchLengths (unroot t) lengths
    makeRooted (WithBranchLengths t lengths) = WithBranchLengths (makeRooted t) lengths

-------------------------- Rooted forests-----------------------------------
data WithRoots t = WithRoots t [NodeId] (IntMap Bool)

instance NFData t => NFData (WithRoots t) where
    rnf (WithRoots tree roots towardsRoot) = rnf tree `seq` rnf roots `seq` rnf towardsRoot

class (IsDirectedAcyclicGraph t, IsForest t) => HasRoots t where
    isRoot :: t -> NodeId -> Bool
    roots :: t -> [NodeId]

    isRoot f n = isSource f n
    roots f = filter (isRoot f) (getNodes f)

instance IsForest f => IsDirectedGraph (WithRoots f) where
    isForward (WithRoots t _ arr    ) b = arr IntMap.! b

instance IsForest f => IsDirectedAcyclicGraph (WithRoots f)

instance IsForest t => HasRoots (WithRoots t) where
    roots (WithRoots _ rs _) = rs
    isRoot (WithRoots _ rs _) node = node `elem` rs

instance HasRoots t => HasRoots (WithLabels t l) where
    roots (WithLabels t _) = roots t
    isRoot (WithLabels t _) node = isRoot t node

instance IsDirectedGraph g => IsDirectedGraph (WithNodeTimes g) where
    isForward (WithNodeTimes g _) e = isForward g e

instance IsDirectedAcyclicGraph g => IsDirectedAcyclicGraph (WithNodeTimes g)

instance HasRoots t => HasRoots (WithNodeTimes t) where
    roots (WithNodeTimes t _)     = roots t
    isRoot (WithNodeTimes t _) node = isRoot t node

instance IsGraph t => IsGraph (WithRoots t) where
    getNodesSet (WithRoots t _ _)                 = getNodesSet t
    getEdgesSet (WithRoots t _ _)                 = getEdgesSet t

    edgesOutOfNodeSet (WithRoots t _ _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (WithRoots t _ _) edgeId           = sourceNode t edgeId
    targetNode (WithRoots t _ _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithRoots t _ _) node         = getNodeAttributes t node
    getEdgeAttributes (WithRoots t _ _) edge         = getEdgeAttributes t edge
    getAttributes (WithRoots t _ _)              = getAttributes t

instance IsForest t => IsForest (WithRoots t) where
    type Unrooted (WithRoots t) = Unrooted t
    type Rooted   (WithRoots t) = WithRoots t

    unroot (WithRoots t _ _) = unroot t
    makeRooted t = t

toward_root rt b = not $ isForward rt b

branchToParent rtree node = find (toward_root rtree) (edgesOutOfNode rtree node)
branchFromParent rtree node = reverseEdge <$> branchToParent rtree node

parentNode rooted_tree n = case branchToParent rooted_tree n of Just b  -> Just $ targetNode rooted_tree b
                                                                Nothing -> Nothing

instance IsDirectedGraph g => IsDirectedGraph (WithBranchLengths g) where
    isForward (WithBranchLengths g _) e = isForward g e

instance IsDirectedAcyclicGraph g => IsDirectedAcyclicGraph (WithBranchLengths g)

instance HasRoots t => HasRoots (WithBranchLengths t) where
    roots (WithBranchLengths tree _) = roots tree
    isRoot (WithBranchLengths t _) node = isRoot t node

instance IsDirectedGraph g => IsDirectedGraph (WithBranchRates g) where
    isForward (WithBranchRates g _) e = isForward g e

instance IsDirectedAcyclicGraph g => IsDirectedAcyclicGraph (WithBranchRates g)

instance (HasNodeTimes t, HasRoots t) => HasRoots (WithBranchRates t) where
    roots (WithBranchRates t _) = roots t
    isRoot (WithBranchRates t _) node = isRoot t node

-------------------------- Forests with node times--------------------------
-- The array stores the node times
data WithNodeTimes t  = WithNodeTimes t (IntMap Double)

instance NFData t => NFData (WithNodeTimes t) where
    rnf (WithNodeTimes tree times) = rnf tree `seq` rnf times

instance IsGraph t => IsGraph (WithNodeTimes t) where
    getNodesSet (WithNodeTimes t _)                     = getNodesSet t
    getEdgesSet (WithNodeTimes t _)                     = getEdgesSet t

    edgesOutOfNodeSet (WithNodeTimes t _) nodeId    = edgesOutOfNodeSet t nodeId
    sourceNode (WithNodeTimes t _) edgeId           = sourceNode t edgeId
    targetNode (WithNodeTimes t _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithNodeTimes t _) node     = getNodeAttributes t node
    getEdgeAttributes (WithNodeTimes t _) edge     = getEdgeAttributes t edge
    getAttributes (WithNodeTimes t _)              = getAttributes t

instance (IsDirectedAcyclicGraph t, IsForest t) => IsForest (WithNodeTimes t) where
    type Unrooted (WithNodeTimes t) = WithBranchLengths (Unrooted t)
    type Rooted   (WithNodeTimes t) = WithNodeTimes (Rooted t)

    unroot tt@(WithNodeTimes t node_heights) = WithBranchLengths (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branchLength tt b))
    makeRooted (WithNodeTimes t node_heights) = WithNodeTimes (makeRooted t) node_heights

class IsDirectedAcyclicGraph g => HasNodeTimes g where
    nodeTime :: g -> Int -> Double
    nodeTimes :: g -> IntMap Double
    modifyNodeTimes :: g -> (Double -> Double) -> g

-- We could separate out modifyNodeTimes out into a separate class CanModifyNodeTimes, like with CanModifyBranchLengths
instance IsDirectedAcyclicGraph g => HasNodeTimes (WithNodeTimes g) where
    nodeTime (WithNodeTimes _ hs) node = hs IntMap.! node
    nodeTimes (WithNodeTimes _ hs) = hs
    modifyNodeTimes (WithNodeTimes tree hs) f = WithNodeTimes tree (fmap f hs)

instance HasNodeTimes t => HasNodeTimes (WithLabels t l) where
    nodeTime (WithLabels tt _) node = nodeTime tt node
    nodeTimes (WithLabels tt _) = nodeTimes tt
    modifyNodeTimes (WithLabels tt ls) f = WithLabels (modifyNodeTimes tt f) ls

instance HasNodeTimes t => HasNodeTimes (WithBranchRates t) where
    nodeTime (WithBranchRates tt _) node = nodeTime tt node
    nodeTimes (WithBranchRates tt _) = nodeTimes tt
    modifyNodeTimes (WithBranchRates tt rs) f = WithBranchRates (modifyNodeTimes tt f) rs

instance HasNodeTimes t => HasBranchRates (WithBranchRates t) where
    branch_rate (WithBranchRates _ rs) node = rs IntMap.! node

instance IsDirectedAcyclicGraph t => HasBranchLengths (WithNodeTimes t) where
    branchLength tree b = branch_duration tree b

branch_duration t b = abs (nodeTime t source - nodeTime t target)
    where source = sourceNode t b
          target = targetNode t b

time_tree topology times = WithNodeTimes topology times

-------------------------- Forests with branch rates------------------------
-- The array stores the branch rates
data WithBranchRates t = WithBranchRates t (IntMap Double)

instance NFData t => NFData (WithBranchRates t) where
    rnf (WithBranchRates tree rates) = rnf tree `seq` rnf rates

instance IsGraph t => IsGraph (WithBranchRates t) where
    getNodesSet (WithBranchRates t _)                 = getNodesSet t
    getEdgesSet (WithBranchRates t _)                 = getEdgesSet t

    edgesOutOfNodeSet (WithBranchRates t _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (WithBranchRates t _) edgeId           = sourceNode t edgeId
    targetNode (WithBranchRates t _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithBranchRates t _) node         = getNodeAttributes t node
    getEdgeAttributes (WithBranchRates t _) edge         = getEdgeAttributes t edge
    getAttributes (WithBranchRates t _)                  = getAttributes t

instance (HasNodeTimes t, IsForest t) => IsForest (WithBranchRates t) where
    type Unrooted (WithBranchRates t) = WithBranchLengths (Unrooted t)
    type Rooted (WithBranchRates t) = WithBranchRates (Rooted t)

    unroot tt@(WithBranchRates t _) = WithBranchLengths (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branchLength tt b))
    makeRooted (WithBranchRates t branchRates) = WithBranchRates (makeRooted t) branchRates


class HasNodeTimes t => HasBranchRates t where
    branch_rate :: t -> Int -> Double

instance (IsDirectedAcyclicGraph t, HasLabels t) => HasLabels (WithNodeTimes t) where
    type instance LabelType (WithNodeTimes t) = LabelType t
    getLabel (WithNodeTimes t _) node          = getLabel t node
    getLabels (WithNodeTimes t _) = getLabels t
    relabel newLabels (WithNodeTimes t nodeHeights) = WithNodeTimes (relabel newLabels t) nodeHeights

instance (HasNodeTimes t, HasLabels t) => HasLabels (WithBranchRates t) where
    type instance LabelType (WithBranchRates t) = LabelType t
    getLabel (WithBranchRates t _) node      = getLabel t node
    getLabels (WithBranchRates t _) = getLabels t
    relabel newLabels (WithBranchRates t branchRates) = WithBranchRates (relabel newLabels t) branchRates

instance HasLabels t => HasLabels (WithRoots t) where
    type instance LabelType (WithRoots t) = LabelType t
    getLabel (WithRoots t _ _) node               = getLabel t node
    getLabels (WithRoots t _ _)                   = getLabels t
    relabel newLabels (WithRoots t roots forward)  = WithRoots (relabel newLabels t) roots forward

instance HasNodeTimes t => HasBranchLengths (WithBranchRates t) where
    branchLength tree b = branch_duration tree b * branch_rate tree b

instance HasBranchLengths t => HasBranchLengths (WithRoots t) where
    branchLength (WithRoots t _ _) b   = branchLength t b

instance CanModifyBranchLengths t => CanModifyBranchLengths (WithRoots t) where
    modifyBranchLengths f (WithRoots t roots forward) = WithRoots (modifyBranchLengths f t) roots forward

rate_time_tree time_tree rates = WithBranchRates time_tree rates

--

allEdgesAfterEdge tree b = b:concatMap (allEdgesAfterEdge tree) (edgesAfterEdge tree b)
allEdgesFromNode tree n = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree n)
allEdgesFromRoots forest = concat [concatMap (allEdgesAfterEdge forest) (edgesOutOfNode forest root) | root <- roots forest]

addRoots roots t = rt
    where check_away_from_root b = (sourceNode rt b `elem` roots) || (or $ fmap (isForward rt) (edgesBeforeEdge rt b))
          nb = numBranches t * 2
          rt = WithRoots t roots (getEdgesSet t & IntMap.fromSet check_away_from_root)

forestFromEdges nodes edges = Forest $ graphFromEdges nodes edges
