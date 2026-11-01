module Tree.Newick.Write where

import Tree.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display

-- We need to handle adding (i) root (ii) labels (iii) branch lengths.
-- Can we do this more generically?
class IsTree t => WriteNewickNode t where
    node_info :: t -> Int -> Text
    branch_info :: t -> (Maybe Int) -> Text

    node_info _ _ = T.empty
    branch_info _ _ = T.empty

get_node_label   t node = T.append (node_info t node) (attributesText $ getNodeAttributes t node)
get_branch_label t branch@(Just edge) = let text = (branch_info t branch) `T.append` (attributesText $ getEdgeAttributes t edge)
                                        in if T.null text then text else T.singleton ':' `T.append` text
get_branch_label t Nothing = T.empty

instance Display l => WriteNewickNode (Tree l) where
    node_info   tree node                         = case getLabel tree node of
                                                      Just label -> quoteLabel (display label)
                                                      Nothing -> T.empty

instance WriteNewickNode t => WriteNewickNode (WithRoots t) where
    node_info (WithRoots tree _ _) = node_info tree
    branch_info (WithRoots tree _ _) = branch_info tree

foreign import ecall "Text:" quoteLabelRaw :: CPPString -> CPPString
quoteLabel l = T.fromCppString $ quoteLabelRaw $ T.toCppString l

instance WriteNewickNode t => WriteNewickNode (WithBranchLengths t) where
    node_info (WithBranchLengths tree lengths) node = node_info tree node

    branch_info blt (Just b) = T.doubleToText (branchLength blt b)
    branch_info _   Nothing  = T.empty

instance (HasRoot t, WriteNewickNode t) => WriteNewickNode (WithNodeTimes t) where
    node_info (WithNodeTimes tree _) node = node_info tree node

    branch_info nht (Just b) = T.doubleToText (branchLength nht b)
    branch_info _   Nothing  = T.empty

instance (HasBranchLengths t, WriteNewickNode t) => WriteNewickNode (WithBranchRates t) where
    node_info (WithBranchRates tree _) node = node_info tree node

    branch_info rtt (Just b) = T.doubleToText (branchLength rtt b)
    branch_info _    Nothing  = T.empty

writeNewick_rooted tree = let nodes = writeNewick_node tree (root tree)
                              attributes = attributesText $ getAttributes tree
                          in if T.null attributes
                             then nodes
                             else T.concat [attributes,T.singleton ' ',nodes]

writeNewick tree = writeNewick_rooted (makeRooted tree)

intercalate2 t ts = foldl1 (\x y -> x `T.append` t `T.append` y) ts

writeNewick_node tree node = write_branches_and_node tree (edgesOutOfNode tree node) node Nothing `T.snoc` ';' where

    write_branches_and_node tree branches node branch = write_branches tree branches `T.append` get_node_label tree node `T.append` get_branch_label tree branch

    write_branches tree branches | null branches = T.empty
    write_branches tree branches | otherwise     = (T.singleton '(') `T.append` text `T.append` (T.singleton ')') where
        text = intercalate2 (T.singleton ',') $ fmap (write_branch tree) $ branches

    write_branch tree branch = write_branches_and_node tree (edgesAfterEdge tree branch) (targetNode tree branch) (Just branch)

-- Serialize the complete wrapper stack so lengths, node times, and rates are retained.
-- These instances are available through Tree without importing the Newick parser.
instance Display l => Show (Tree l) where
    show = T.unpack . writeNewick

instance WriteNewickNode t => Show (WithRoots t) where
    show = T.unpack . writeNewick

instance (HasRoot (Rooted t), WriteNewickNode (Rooted t), IsTree t) => Show (WithBranchLengths t) where
    show = T.unpack . writeNewick

instance (HasRoot (Rooted t), WriteNewickNode (Rooted t), IsTree t) => Show (WithNodeTimes t) where
    show = T.unpack . writeNewick

instance (HasRoot (Rooted t), WriteNewickNode (Rooted t), HasBranchLengths (Rooted t), IsTree t)
      => Show (WithBranchRates t) where
    show = T.unpack . writeNewick
